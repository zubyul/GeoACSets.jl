"""
    GeoACSets

Categorical data structures (ACSets) with geospatial capabilities.
OPTIMIZED for maximum performance.

Key insight: Use morphisms for structural navigation, geometry for filtering.
- O(1) traversal through spatial hierarchies via `incident`/`subpart`
- R-tree indexed spatial queries (10-100x faster)
- Materialized path caching for deep traversals
- Batched geometry operations (reduced FFI overhead)

## Performance Characteristics

| Operation                    | Naive      | Optimized     |
|------------------------------|------------|---------------|
| Spatial filter (selective)   | O(n)       | O(log n + k)  |
| Spatial join                 | O(n*m)     | O(n log m + k)|
| Deep traversal (cached)      | O(d)       | O(1)          |
| Batch intersects             | n × FFI    | 1 × FFI + n   |

## Quick Start

```julia
using GeoACSets

city = SpatialCity{LibGEOS.Polygon, String, Float64}()

# Add structure
r = add_part!(city, :Region, region_name="Downtown", region_geom=poly)
# ... add districts, parcels, buildings

# FAST: R-tree indexed query
nearby = spatial_filter_indexed(city, :Building, :footprint, 
    query_poly, LibGEOS.intersects)

# FAST: Cached deep traversal  
regions = region_of_buildings_batch(city, building_ids)

# Remember to invalidate after mutations!
rem_part!(city, :Building, b)
invalidate_indices!(city)
invalidate_paths!(city)
```
"""
module GeoACSets

using Reexport

@reexport using ACSets
using GeoInterface

# Try to load LibGEOS, but don't fail if unavailable
const HAS_LIBGEOS = try
    @eval using LibGEOS
    true
catch
    false
end

# Core exports
export SpatialCity, SchSpatialCity,
       SpatialGraph, SchSpatialGraph,
       ParcelAdjacency, SchParcelAdjacency,
       HAS_LIBGEOS

# Predicate exports  
export spatial_filter, spatial_join

# Traversal exports
export buildings_in_region, region_of_building,
       parcels_in_district, district_of_parcel,
       buildings_in_district, parcels_in_region,
       district_of_building, region_of_parcel,
       traverse_up, traverse_down,
       neighbors, edges_between

# NEW: Optimized exports
export spatial_filter_indexed, spatial_join_indexed, nearest_k,
       batch_intersects, invalidate_indices!,
       traverse_up_cached, traverse_down_cached,
       batch_traverse_up, region_of_buildings_batch,
       invalidate_paths!

# Core modules
include("schemas.jl")
include("predicates.jl")
include("traversal.jl")

# Performance modules
include("spatial_index.jl")
include("materialized_paths.jl")

# =============================================================================
# Convenience: Combined invalidation
# =============================================================================

"""
    invalidate_all!(acs)

Invalidate all caches after mutation. Call after add_part!/rem_part!/set_subpart!
"""
function invalidate_all!(acs)
    invalidate_indices!(acs)
    invalidate_paths!(acs)
end

export invalidate_all!

end # module
