package com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.repository;

import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.converter.Cuisine2EntityConverter;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.converter.CuisineEntity2DefaultConverter;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driven.CuisineRepository;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.internal.entities.Cuisine;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class CuisineRepositoryImpl implements CuisineRepository {


    private CuisineJPARepository cuisineJPARepository;
    private Cuisine2EntityConverter cuisine2EntityConverter;
    private CuisineEntity2DefaultConverter cuisineEntity2DefaultConverter;

    @Autowired
    public CuisineRepositoryImpl(CuisineJPARepository cuisineJPARepository, Cuisine2EntityConverter cuisine2EntityConverter, CuisineEntity2DefaultConverter cuisineEntity2DefaultConverter) {
        this.cuisineJPARepository = cuisineJPARepository;
        this.cuisine2EntityConverter = cuisine2EntityConverter;
        this.cuisineEntity2DefaultConverter = cuisineEntity2DefaultConverter;
    }

    @Override
    public List<Cuisine> findByName(String name) {
        throw new UnsupportedOperationException("Not implemented");
    }

    @Override
    public Cuisine save(Cuisine entity) {
        CuisineEntity cuisineEntity = cuisine2EntityConverter.convert(entity);
        cuisineEntity = this.cuisineJPARepository.save(cuisineEntity);
        entity = cuisineEntity2DefaultConverter.convert(cuisineEntity);
        return entity;
    }

    @Override
    public Boolean existsByName(String name) {
        return this.cuisineJPARepository.existsByName(name);
    }

    @Override
    public List<Cuisine> findAll() {
        throw new UnsupportedOperationException("Not implemented");
    }

    @Override
    public Optional<Cuisine> findById(Long cuisineId) {
        Optional<CuisineEntity> cuisineEntity = this.cuisineJPARepository.findById(cuisineId);
        Optional<Cuisine> entity = cuisineEntity.isPresent() ?
                Optional.of(cuisineEntity2DefaultConverter.convert(cuisineEntity.get())) : Optional.empty();
        return entity;
    }
}
