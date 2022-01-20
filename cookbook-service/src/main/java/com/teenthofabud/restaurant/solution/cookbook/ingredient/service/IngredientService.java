package com.teenthofabud.restaurant.solution.cookbook.ingredient.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientException;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientForm;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface IngredientService {

    public Set<IngredientVo> retrieveAllByNaturalOrdering();

    public IngredientVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws IngredientException;

    public List<IngredientVo> retrieveAllMatchingDetailsByRecipeId(String recipeId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws IngredientException;

    public List<IngredientVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                   Optional<String> optionalDescription,
                                                                   Optional<String> optionalProductId,
                                                                   Optional<String> optionalQuantityAmount,
                                                                   Optional<String> optionalQuantityUnitId) throws IngredientException;

    public String createIngredient(IngredientForm form) throws IngredientException;

    public void updateIngredient(String id, IngredientForm form) throws IngredientException;

    public void deleteIngredient(String id) throws IngredientException;

    public void applyPatchOnIngredient(String id, List<PatchOperationForm> patches) throws IngredientException;

}
