package com.teenthofabud.restaurant.solution.cookbook.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.converter.CuisineEntity2VoConverter;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.converter.IngredientEntity2VoConverter;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.converter.RecipeEntity2VoConverter;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import tech.units.indriya.unit.BaseUnit;
import tech.units.indriya.unit.UnitDimension;

import javax.measure.Unit;
import javax.measure.quantity.Mass;
import javax.measure.quantity.Time;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

@Slf4j
@Component
public class CookbookServiceHelper {

    private static final Unit<Time> H = new BaseUnit("h", "Hour", UnitDimension.TIME);
    private static final Unit<Time> M = new BaseUnit("m", "Minute", UnitDimension.TIME);
    private static final Unit<Time> S = new BaseUnit("s", "Second", UnitDimension.TIME);
    private static final Unit<Mass> KG = new BaseUnit("kg", "Kilogram", UnitDimension.MASS);
    private static final Unit<Mass> G = new BaseUnit("g", "Gram", UnitDimension.MASS);


    private CuisineEntity2VoConverter cuisineEntity2VoConverter;
    private RecipeEntity2VoConverter recipeEntity2VoConverter;
    private IngredientEntity2VoConverter ingredientEntity2VoConverter;

    @Autowired
    public void setCuisineEntity2VoConverter(CuisineEntity2VoConverter cuisineEntity2VoConverter) {
        this.cuisineEntity2VoConverter = cuisineEntity2VoConverter;
    }

    @Autowired
    public void setRecipeEntity2VoConverter(RecipeEntity2VoConverter recipeEntity2VoConverter) {
        this.recipeEntity2VoConverter = recipeEntity2VoConverter;
    }

    @Autowired
    public void setIngredientEntity2VoConverter(IngredientEntity2VoConverter ingredientEntity2VoConverter) {
        this.ingredientEntity2VoConverter = ingredientEntity2VoConverter;
    }

    public List<CuisineVo> cuisineEntity2DetailedVo(List<? extends CuisineEntity> cuisineEntityList) {
        List<CuisineVo> cuisineDetailsList = new LinkedList<>();
        if(cuisineEntityList != null && !cuisineEntityList.isEmpty()) {
            for(CuisineEntity entity : cuisineEntityList) {
                CuisineVo vo = this.cuisineEntity2DetailedVo(entity);
                cuisineDetailsList.add(vo);
            }
        }
        return cuisineDetailsList;
    }

    public CuisineVo cuisineEntity2DetailedVo(CuisineEntity cuisineEntity) {
        if(cuisineEntity != null) {
            CuisineVo vo = cuisineEntity2VoConverter.convert(cuisineEntity);
            log.debug("Converting {} to {}", cuisineEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "cuisine entity is null" });
    }

    public boolean isTimeIdValid(String timeId) {
        Optional<Unit<Time>> optionalTimeId = this.parseTimeId(timeId);
        return optionalTimeId.isPresent();
    }

    public Optional<Unit<Time>> parseTimeId(String timeId) {
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(timeId))) {
            log.debug("timeId is empty");
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object [] { "timeId is empty" });
        }
        Optional<Unit<Time>> optionalTimeId = H.getSymbol().compareTo(timeId.toLowerCase()) == 0 ? Optional.of(H)
                : M.getSymbol().compareTo(timeId.toLowerCase()) == 0 ? Optional.of(M)
                : S.getSymbol().compareTo(timeId.toLowerCase()) == 0 ? Optional.of(S)
                : Optional.empty();
        return optionalTimeId;
    }

    public boolean isWeightCodeValid(String weightCode) {
        Optional<Unit<Mass>> optionalWeight = this.parseWeightCode(weightCode);
        return optionalWeight.isPresent();
    }

    public Optional<Unit<Mass>> parseWeightCode(String weightCode) {
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(weightCode))) {
            log.debug("weight code is empty");
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object [] { "weight code is empty" });
        }
        Optional<Unit<Mass>> optionalWeight = KG.getSymbol().compareTo(weightCode.toLowerCase()) == 0 ? Optional.of(KG)
                : G.getSymbol().compareTo(weightCode.toLowerCase()) == 0 ? Optional.of(G) :Optional.empty();
        return optionalWeight;
    }

    public List<RecipeVo> recipeEntity2DetailedVo(List<? extends RecipeEntity> recipeEntityList) {
        List<RecipeVo> recipeDetailsList = new ArrayList<>(recipeEntityList.size());
        for(RecipeEntity entity : recipeEntityList) {
            RecipeVo vo = this.recipeEntity2DetailedVo(entity);
            recipeDetailsList.add(vo);
        }
        return recipeDetailsList;
    }

    public RecipeVo recipeEntity2DetailedVo(RecipeEntity recipeEntity) {
        if(recipeEntity != null) {
            RecipeVo vo = recipeEntity2VoConverter.convert(recipeEntity);
            log.debug("Converting {} to {}", recipeEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "recipe entity is null" });
    }

    public List<IngredientVo> ingredientEntity2DetailedVo(List<? extends IngredientEntity> ingredientEntityList) {
        List<IngredientVo> ingredientDetailsList = new ArrayList<>(ingredientEntityList.size());
        for(IngredientEntity entity : ingredientEntityList) {
            IngredientVo vo = this.ingredientEntity2DetailedVo(entity);
            ingredientDetailsList.add(vo);
        }
        return ingredientDetailsList;
    }

    public IngredientVo ingredientEntity2DetailedVo(IngredientEntity ingredientEntity) {
        if(ingredientEntity != null) {
            IngredientVo vo = ingredientEntity2VoConverter.convert(ingredientEntity);
            log.debug("Converting {} to {}", ingredientEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "ingredient entity is null" });
    }

}
