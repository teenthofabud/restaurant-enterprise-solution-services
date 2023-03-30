package com.teenthofabud.restaurant.solution.cookbook.cuisine.repository;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.Cuisine;
import java.util.List;


public interface CuisineRepository {

    List<Cuisine> findByName(String name);

    public Cuisine save(Cuisine entity);

    Boolean existsByName(String name);
    
}
