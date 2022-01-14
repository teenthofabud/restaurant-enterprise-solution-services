package com.teenthofabud.restaurant.solution.cookbook.ingredient.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.service.RecipeService;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientDto;
import com.teenthofabud.restaurant.solution.cookbook.utils.CookbookServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class IngredientDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private Validator productIdValidator;
    private CookbookServiceHelper cookbookServiceHelper;
    private RecipeService recipeService;

    @Value("#{'${res.cookbook.ingredient.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Autowired
    public void setRecipeService(RecipeService recipeService) {
        this.recipeService = recipeService;
    }

    @Autowired
    @Qualifier("productIdValidator")
    public void setProductIdValidator(Validator productIdValidator) {
        this.productIdValidator = productIdValidator;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(IngredientDto.class);
    }

    /**
     * Order in which the fields are being validated are important
     */

    @Override
    public void validate(Object target, Errors errors) {
        IngredientDto dto = (IngredientDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("IngredientDto.name is invalid");
        }

        Optional<String> optQuantityAmount = dto.getQuantityAmount();
        if(!fieldsToEscape.contains("quantityAmount") && optQuantityAmount.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optQuantityAmount.get()))) {
            errors.rejectValue("quantityAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("IngredientDto.quantityAmount is empty");
            return;
        } else if(!fieldsToEscape.contains("quantityAmount") && optQuantityAmount.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optQuantityAmount.get()))) {
            try {
                Double duration = Double.parseDouble(optQuantityAmount.get());
                if(duration <= 0.0d) {
                    errors.rejectValue("quantityAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    log.debug("IngredientDto.quantityAmount is invalid");
                    return;
                }
            } catch (NumberFormatException e) {
                errors.rejectValue("quantityAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                log.debug("IngredientDto.quantityAmount is invalid");
                return;
            }
        }

        Optional<String> optQuantityUnitId = dto.getQuantityUnitId();
        if(!fieldsToEscape.contains("quantityUnitId") && optQuantityUnitId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optQuantityUnitId.get()))) {
            errors.rejectValue("quantityUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("IngredientDto.quantityUnitId is empty");
            return;
        } else if(!fieldsToEscape.contains("quantityUnitId") && optQuantityUnitId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optQuantityUnitId.get()))) {
            if(!cookbookServiceHelper.isWeightCodeValid(optQuantityUnitId.get())) {
                log.debug("IngredientDto.quantityUnitId is invalid");
                errors.rejectValue("quantityUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optProductId = dto.getProductId();
        if(!fieldsToEscape.contains("productId") && optProductId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optProductId.get()))) {
            errors.rejectValue("productId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("IngredientDto.productId is invalid");
            return;
        } else if(!fieldsToEscape.contains("productId") && optProductId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optProductId.get()))) {
            String productId = optProductId.get();
            Errors err = new DirectFieldBindingResult(productId, "IngredientDto");
            productIdValidator.validate(productId, err);
            if(err.hasErrors()) {
                log.debug("IngredientDto.productId is invalid");
                CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("IngredientDto error detail: {}", ec);
                errors.rejectValue("productId", ec.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                log.debug("IngredientDto.active is invalid");
                return;
            }
        }

        Optional<String> optRecipeId = dto.getRecipeId();
        if(!fieldsToEscape.contains("recipeId") && optRecipeId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optRecipeId.get()))) {
            errors.rejectValue("recipeId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("IngredientDto.recipeId is invalid");
            return;
        } else if(!fieldsToEscape.contains("recipeId") && optRecipeId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optRecipeId.get()))) {
            String recipeId = optRecipeId.get();
            try {
                Long.parseLong(recipeId);
            } catch (NumberFormatException e) {
                log.debug("IngredientDto.recipeId is invalid");
                errors.rejectValue("recipeId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                RecipeVo recipeVo = recipeService.retrieveDetailsById(recipeId, Optional.of(TOABCascadeLevel.ONE));
                if(!recipeVo.getActive()) {
                    log.debug("IngredientDto.recipeId is inactive");
                    errors.rejectValue("recipeId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (RecipeException e) {
                log.debug("IngredientDto.recipeId is invalid");
                errors.rejectValue("recipeId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
