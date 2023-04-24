package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driven;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.entities.Cuisine;
import java.util.List;


public interface CuisineRepository {

    List<Cuisine> findByName(String name);

    public Cuisine save(Cuisine entity);

    Boolean existsByName(String name);
    
}
