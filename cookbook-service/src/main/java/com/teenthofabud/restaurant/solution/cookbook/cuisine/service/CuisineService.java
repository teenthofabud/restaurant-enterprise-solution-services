package com.teenthofabud.restaurant.solution.cookbook.cuisine.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineForm;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface CuisineService {

    public Set<CuisineVo> retrieveAllByNaturalOrdering();
    
    public CuisineVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CuisineException;

    public List<CuisineVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                                    Optional<String> optionalDescription) throws CuisineException;

    public String createCuisine(CuisineForm form) throws CuisineException;

    public void updateCuisine(String id, CuisineForm form) throws CuisineException;

    public void deleteCuisine(String id) throws CuisineException;

    public void applyPatchOnCuisine(String id, List<PatchOperationForm> patches) throws CuisineException;

}
