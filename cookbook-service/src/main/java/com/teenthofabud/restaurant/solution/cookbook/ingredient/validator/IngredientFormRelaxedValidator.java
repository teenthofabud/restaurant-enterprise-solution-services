package com.teenthofabud.restaurant.solution.cookbook.ingredient.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeException;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.service.RecipeService;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientForm;
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
public class IngredientFormRelaxedValidator implements RelaxedValidator<IngredientForm>  {

    private List<String> fieldsToEscape;
    private RecipeService recipeService;
    private CookbookServiceHelper cookbookServiceHelper;
    private Validator productIdValidator;


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
    public Boolean validateLoosely(IngredientForm form, Errors errors) {

        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("IngredientForm.name is empty");
            errors.rejectValue("name", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return false;
        }
        log.debug("IngredientForm.name is valid");
        
        if(!fieldsToEscape.contains("quantityAmount") && form.getQuantityAmount() != null && form.getQuantityAmount() <= 0.0d) {
            errors.rejectValue("quantityAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("IngredientForm.quantityAmount is invalid");
            return false;
        }
        log.debug("IngredientForm.quantityAmount is valid");

        if(!fieldsToEscape.contains("quantityUnitId") && form.getQuantityUnitId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getQuantityUnitId()))) {
            errors.rejectValue("quantityUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("IngredientForm.quantityUnitId is empty");
            return false;
        } else if(!fieldsToEscape.contains("quantityUnitId") && form.getQuantityUnitId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getQuantityUnitId()))) {
            if(!cookbookServiceHelper.isWeightCodeValid(form.getQuantityUnitId())) {
                log.debug("IngredientForm.quantityUnitId is invalid");
                errors.rejectValue("quantityUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("IngredientForm.quantityUnitId is valid");

        if(!fieldsToEscape.contains("productId") && form.getProductId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getProductId()))) {
            log.debug("IngredientForm.productId is empty");
            errors.rejectValue("productId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("productId") && form.getProductId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getProductId()))){
            Errors err = new DirectFieldBindingResult(form.getProductId(), "IngredientForm");
            productIdValidator.validate(form.getProductId(), err);
            if(err.hasErrors()) {
                log.debug("IngredientForm.productId is invalid");
                CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("IngredientForm error detail: {}", ec);
                errors.rejectValue("productId", ec.name());
                return false;
            }
        }
        log.debug("IngredientForm.productId is valid");

        if(!fieldsToEscape.contains("recipeId") && form.getRecipeId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getRecipeId()))) {
            log.debug("IngredientForm.recipeId is empty");
            errors.rejectValue("recipeId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("recipeId") && form.getRecipeId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getRecipeId()))){
            String recipeId = form.getRecipeId();
            try {
                RecipeVo recipeVo = recipeService.retrieveDetailsById(recipeId, Optional.of(TOABCascadeLevel.ONE));
                if(!recipeVo.getActive()) {
                    log.debug("IngredientForm.recipeId is inactive");
                    errors.rejectValue("recipeId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (RecipeException e) {
                log.debug("IngredientForm.recipeId is invalid");
                errors.rejectValue("recipeId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("IngredientForm.recipeId is valid");
        return true;
    }
}
