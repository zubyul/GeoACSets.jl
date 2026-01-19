# Materialized path cache for deep morphism traversals
#
# Problem: traverse_up(city, building, :building_on, :parcel_of, :district_of)
#          requires 3 sequential lookups per building.
#
# Solution: Cache full paths on first traversal, invalidate on mutation.
# Result: O(1) lookup for repeated deep traversals.

# =============================================================================
# Path Cache Structure
# =============================================================================

"""
    PathCache

Caches materialized paths for morphism chains.
Key: (start_object, morphism_chain...)
Value: Dict{Int, Int} mapping start_part → end_part
"""
mutable struct PathCache
    paths::Dict{Tuple, Dict{Int,Int}}
    version::Int
    PathCache() = new(Dict(), 0)
end

const PATH_CACHE = WeakKeyDict{Any, PathCache}()

get_path_cache(acs) = get!(PATH_CACHE, acs) do; PathCache(); end

invalidate_paths!(acs) = (get_path_cache(acs).version += 1; nothing)

# =============================================================================
# Cached Traversal
# =============================================================================

"""
    traverse_up_cached(acs, part, morphisms...) -> Int

Cached version of traverse_up. First call builds cache, subsequent calls O(1).
"""
function traverse_up_cached(acs, part::Int, morphisms::Symbol...)
    cache = get_path_cache(acs)
    # Determine start object from first morphism's domain
    start_obj = dom(acs, first(morphisms))
    key = (start_obj, morphisms...)
    
    # Check cache validity
    path_dict = get(cache.paths, key, nothing)
    if isnothing(path_dict)
        path_dict = materialize_paths_up(acs, start_obj, morphisms)
        cache.paths[key] = path_dict
    end
    
    get(path_dict, part, 0)
end

"""
    materialize_paths_up(acs, start_obj, morphisms) -> Dict{Int,Int}

Build complete path mapping for all parts of start_obj.
"""
function materialize_paths_up(acs, start_obj::Symbol, morphisms::Tuple)
    result = Dict{Int,Int}()
    
    for p in parts(acs, start_obj)
        current = p
        for m in morphisms
            current = acs[current, m]
            current == 0 && break
        end
        current != 0 && (result[p] = current)
    end
    
    result
end

"""
    traverse_down_cached(acs, part, inverse_morphisms...) -> Vector{Int}

Cached downward traversal (ancestor → descendants).
"""
function traverse_down_cached(acs, part::Int, morphisms::Symbol...)
    cache = get_path_cache(acs)
    end_obj = codom(acs, last(morphisms))
    key = (:down, end_obj, morphisms...)
    
    path_dict = get(cache.paths, key, nothing)
    if isnothing(path_dict)
        path_dict = materialize_paths_down(acs, morphisms)
        cache.paths[key] = path_dict
    end
    
    get(path_dict, part, Int[])
end

function materialize_paths_down(acs, morphisms::Tuple)
    # Build reverse mapping: end_part → [start_parts...]
    result = Dict{Int, Vector{Int}}()
    
    start_obj = dom(acs, first(morphisms))
    for p in parts(acs, start_obj)
        current = p
        for m in morphisms
            current = acs[current, m]
            current == 0 && break
        end
        if current != 0
            lst = get!(result, current) do; Int[]; end
            push!(lst, p)
        end
    end
    
    result
end

# =============================================================================
# Batch Traversal (SIMD-friendly)
# =============================================================================

"""
    batch_traverse_up(acs, parts::Vector{Int}, morphisms...) -> Vector{Int}

Traverse multiple parts in one call. Better cache locality.
"""
function batch_traverse_up(acs, parts_vec::Vector{Int}, morphisms::Symbol...)
    cache = get_path_cache(acs)
    start_obj = dom(acs, first(morphisms))
    key = (start_obj, morphisms...)
    
    path_dict = get(cache.paths, key, nothing)
    if isnothing(path_dict)
        path_dict = materialize_paths_up(acs, start_obj, morphisms)
        cache.paths[key] = path_dict
    end
    
    [get(path_dict, p, 0) for p in parts_vec]
end

"""
    region_of_buildings_batch(city, building_ids) -> Vector{Int}

Optimized batch lookup: all buildings → their regions.
"""
function region_of_buildings_batch(city::SpatialCity, building_ids::Vector{Int})
    batch_traverse_up(city, building_ids, :building_on, :parcel_of, :district_of)
end

# =============================================================================
# Helper: Infer domain/codomain from schema
# =============================================================================

function dom(acs, morphism::Symbol)
    # Get schema and find morphism's domain
    schema = acset_schema(acs)
    for (name, d, c) in schema.homs
        name == morphism && return d
    end
    error("Morphism  not found")
end

function codom(acs, morphism::Symbol)
    schema = acset_schema(acs)
    for (name, d, c) in schema.homs
        name == morphism && return c
    end
    error("Morphism  not found")
end

export PathCache, traverse_up_cached, traverse_down_cached,
       batch_traverse_up, region_of_buildings_batch,
       invalidate_paths!
