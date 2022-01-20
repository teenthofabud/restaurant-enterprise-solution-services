package com.teenthofabud.restaurant.solution.cookbook.ingredient.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.repository.RecipeRepository;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientDto;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import com.teenthofabud.restaurant.solution.cookbook.utils.CookbookServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class IngredientDto2EntityConverter implements ComparativePatchConverter<IngredientDto, IngredientEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 13;

    private List<String> fieldsToEscape;
    private CookbookServiceHelper cookbookServiceHelper;

    @Value("#{'${res.cookbook.ingredient.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private RecipeRepository recipeRepository;

    @Autowired
    public void setRecipeRepository(RecipeRepository recipeRepository) {
        this.recipeRepository = recipeRepository;
    }

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Override
    public void compareAndMap(IngredientDto dto, IngredientEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optRecipeId = dto.getRecipeId();
        if(!fieldsToEscape.contains("recipeId") && optRecipeId.isPresent()) {
            Long recipeId = Long.parseLong(optRecipeId.get());
            Optional<RecipeEntity> optRecipeEntity = recipeRepository.findById(recipeId);
            actualEntity.setRecipe(optRecipeEntity.get());
            changeSW[i++] = true;
            log.debug("IngredientDto.recipeId is valid");
        }
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("IngredientDto.name is valid");
        }
        Optional<String> optProductId = dto.getProductId();
        if(!fieldsToEscape.contains("productId") && optProductId.isPresent()) {
            actualEntity.setProductId(optProductId.get());
            changeSW[i++] = true;
            log.debug("IngredientDto.productId is valid");
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("IngredientDto.description is valid");
        }
        Optional<String> optQuantityAmount = dto.getQuantityAmount();
        if(!fieldsToEscape.contains("quantityAmount") && optQuantityAmount.isPresent()) {
            Double quantityAmount = Double.parseDouble(optQuantityAmount.get());
            actualEntity.setQuantityAmount(quantityAmount);
            changeSW[i++] = true;
            log.debug("IngredientDto.quantityAmount is valid");
        }
        Optional<String> optQuantityUnitId = dto.getQuantityUnitId();
        if(!fieldsToEscape.contains("quantityUnitId") && optQuantityUnitId.isPresent()) {
            actualEntity.setQuantityUnitId(optQuantityUnitId.get());
            changeSW[i++] = true;
            log.debug("IngredientDto.quantityUnitId is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("IngredientDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided IngredientDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided IngredientDto attributes are valid");
    }

}
