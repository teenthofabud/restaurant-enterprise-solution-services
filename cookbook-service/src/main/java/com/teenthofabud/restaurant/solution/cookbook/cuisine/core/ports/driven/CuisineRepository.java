package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driven;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.entities.Cuisine;
import java.util.List;
import java.util.Optional;


public interface CuisineRepository {

    List<Cuisine> findByName(String name);

    Cuisine save(Cuisine entity);

    Boolean existsByName(String name);

    List<Cuisine> findAll();

    Optional<Cuisine> findById(Long cuisineId);
}
