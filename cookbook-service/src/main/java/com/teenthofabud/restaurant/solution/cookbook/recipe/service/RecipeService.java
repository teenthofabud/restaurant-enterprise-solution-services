package com.teenthofabud.restaurant.solution.cookbook.recipe.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface RecipeService {

    public Set<RecipeVo> retrieveAllByNaturalOrdering();

    public RecipeVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws RecipeException;

    public List<RecipeVo> retrieveAllMatchingDetailsByCuisineId(String cuisineId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws RecipeException;

    public List<RecipeVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                Optional<String> optionalDescription,
                                                                Optional<String> optionalInstructions,
                                                                Optional<String> optionalCookingMethod,
                                                                Optional<String> optionalItemId,
                                                                Optional<String> optionalNumberOfServings,
                                                                Optional<String> optionalPreparationTimeDuration,
                                                                Optional<String> optionalPreparationTimeUnitId,
                                                                Optional<String> optionalCookingTimeDuration,
                                                                Optional<String> optionalCookingTimeUnitId,
                                                                Optional<String> optionalPortionSizeAmount,
                                                                Optional<String> optionalPortionSizeUnitId) throws RecipeException;

    public String createRecipe(RecipeForm form) throws RecipeException;

    public void updateRecipe(String id, RecipeForm form) throws RecipeException;

    public void deleteRecipe(String id) throws RecipeException;

    public void applyPatchOnRecipe(String id, List<PatchOperationForm> patches) throws RecipeException;

}
