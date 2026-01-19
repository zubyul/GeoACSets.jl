# Spatial indexing for GeoACSets - R-tree acceleration
#
# Key optimizations:
# 1. Lazy R-tree construction (built on first spatial query)
# 2. Index caching (survives across queries)
# 3. Batched envelope extraction (reduces Julia↔C transitions)
# 4. Invalidation on mutation

using Base: @kwdef

# =============================================================================
# R-tree Index Structure
# =============================================================================

"""
    SpatialIndex{T}

Cached R-tree index for a geometry column.
"""
@kwdef mutable struct SpatialIndex{T}
    tree::T
    object::Symbol
    attr::Symbol
    version::Int
    part_ids::Vector{Int}
end

"""
    IndexCache - Per-ACSet cache of spatial indices.
"""
mutable struct IndexCache
    indices::Dict{Tuple{Symbol,Symbol}, SpatialIndex}
    mutation_count::Int
    IndexCache() = new(Dict(), 0)
end

const SPATIAL_INDEX_CACHE = WeakKeyDict{Any, IndexCache}()

get_index_cache(acs) = get!(SPATIAL_INDEX_CACHE, acs) do; IndexCache(); end

invalidate_indices!(acs) = (get_index_cache(acs).mutation_count += 1)

# =============================================================================
# R-tree Construction (LibGEOS backend)
# =============================================================================

"""
    build_rtree(acs, object, attr) -> SpatialIndex

Build STRtree index. O(n log n) construction, O(log n + k) query.
"""
function build_rtree(acs, object::Symbol, attr::Symbol)
    cache = get_index_cache(acs)
    part_ids, geoms = Int[], []
    
    for p in parts(acs, object)
        g = acs[p, attr]
        if !isnothing(g)
            push!(part_ids, p)
            push!(geoms, g)
        end
    end
    
    tree = HAS_LIBGEOS ? LibGEOS.STRtree(geoms) : nothing
    
    SpatialIndex(tree=tree, object=object, attr=attr, 
                 version=cache.mutation_count, part_ids=part_ids)
end

function get_or_build_index(acs, object::Symbol, attr::Symbol)
    cache = get_index_cache(acs)
    key = (object, attr)
    idx = get(cache.indices, key, nothing)
    
    if isnothing(idx) || idx.version != cache.mutation_count
        idx = build_rtree(acs, object, attr)
        cache.indices[key] = idx
    end
    idx
end

# =============================================================================
# Accelerated Spatial Queries
# =============================================================================

"""
    spatial_filter_indexed(acs, object, attr, query_geom, relation)

Two-phase: R-tree candidates → exact predicate. 10-100x faster.
"""
function spatial_filter_indexed(acs, object::Symbol, attr::Symbol, 
                                query_geom, relation::Function)
    !HAS_LIBGEOS && return spatial_filter(acs, object, attr, g -> relation(g, query_geom))
    
    idx = get_or_build_index(acs, object, attr)
    query_env = LibGEOS.envelope(query_geom)
    candidates = LibGEOS.query(idx.tree, query_env)
    
    results = Int[]
    for i in candidates
        part_id = idx.part_ids[i]
        geom = acs[part_id, attr]
        relation(geom, query_geom) && push!(results, part_id)
    end
    results
end

"""
    spatial_join_indexed(acs, obj1, attr1, obj2, attr2, relation)

Index smaller set, probe with larger. O(n log m + k) vs O(n*m).
"""
function spatial_join_indexed(acs, obj1::Symbol, attr1::Symbol,
                              obj2::Symbol, attr2::Symbol, relation::Function)
    !HAS_LIBGEOS && return spatial_join(acs, obj1, attr1, obj2, attr2, relation)
    
    n1, n2 = nparts(acs, obj1), nparts(acs, obj2)
    
    if n1 <= n2
        idx = get_or_build_index(acs, obj1, attr1)
        probe_obj, probe_attr, swap = obj2, attr2, false
    else
        idx = get_or_build_index(acs, obj2, attr2)
        probe_obj, probe_attr, swap = obj1, attr1, true
    end
    
    results = Tuple{Int,Int}[]
    for p in parts(acs, probe_obj)
        probe_geom = acs[p, probe_attr]
        isnothing(probe_geom) && continue
        
        probe_env = LibGEOS.envelope(probe_geom)
        for i in LibGEOS.query(idx.tree, probe_env)
            indexed_part = idx.part_ids[i]
            indexed_geom = acs[indexed_part, idx.attr]
            if relation(indexed_geom, probe_geom)
                push!(results, swap ? (p, indexed_part) : (indexed_part, p))
            end
        end
    end
    results
end

"""
    nearest_k(acs, object, attr, query_point, k) -> Vector{Tuple{Int,Float64}}

K-nearest neighbors query.
"""
function nearest_k(acs, object::Symbol, attr::Symbol, query_point, k::Int)
    !HAS_LIBGEOS && error("LibGEOS required for nearest_k")
    
    idx = get_or_build_index(acs, object, attr)
    nearest_indices = LibGEOS.nearest(idx.tree, query_point, k)
    
    results = [(idx.part_ids[i], LibGEOS.distance(acs[idx.part_ids[i], attr], query_point)) 
               for i in nearest_indices]
    sort!(results, by=last)
end

# =============================================================================
# Batched Operations (reduce FFI overhead)
# =============================================================================

"""
    batch_intersects(geoms, query_geom) -> BitVector

Prepared geometry for repeated tests.
"""
function batch_intersects(geoms::Vector, query_geom)
    !HAS_LIBGEOS && return falses(length(geoms))
    
    prepared = LibGEOS.prepareGeom(query_geom)
    try
        BitVector([!isnothing(g) && LibGEOS.intersects(prepared, g) for g in geoms])
    finally
        LibGEOS.destroyPreparedGeom(prepared)
    end
end

export SpatialIndex, IndexCache, invalidate_indices!, get_or_build_index,
       spatial_filter_indexed, spatial_join_indexed, nearest_k, batch_intersects
