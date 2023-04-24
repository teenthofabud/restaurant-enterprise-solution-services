package com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.repository;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driven.CuisineRepository;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.entities.Cuisine;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class CuisineRepositoryImpl implements CuisineRepository {

    private CuisineJPARepository cuisineJPARepository;

    @Autowired
    public CuisineRepositoryImpl(CuisineJPARepository cuisineJPARepository) {
        this.cuisineJPARepository = cuisineJPARepository;
    }

    @Override
    public List<Cuisine> findByName(String name) {
        /*List<CuisineEntity> cuisineEntityList =  cuisineJPARepository.findByName(name);
        Cuisine cuisine = new Cuisine();
        return cuisineEntityList;*/
        throw new UnsupportedOperationException("Not implemented");
    }

    @Override
    public Cuisine save(Cuisine entity) {
        return null;
    }

    @Override
    public Boolean existsByName(String name) {
        return null;
    }
}
