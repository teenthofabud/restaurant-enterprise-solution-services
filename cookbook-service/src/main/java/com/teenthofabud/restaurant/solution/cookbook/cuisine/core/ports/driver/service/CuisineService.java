package com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineRequest;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineResponse;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public interface CuisineService {

    /*public Set<CuisineResponse> retrieveAllByNaturalOrdering();*/
    
    public CuisineResponse retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CuisineException;

    /*public List<CuisineResponse> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                      Optional<String> optionalDescription) throws CuisineException;*/

    public String createCuisine(CuisineRequest form) throws CuisineException;

    public void updateCuisine(String id, CuisineRequest form) throws CuisineException;

    /*public void deleteCuisine(String id) throws CuisineException;

    public void applyPatchOnCuisine(String id, List<PatchOperationForm> patches) throws CuisineException;*/

}
