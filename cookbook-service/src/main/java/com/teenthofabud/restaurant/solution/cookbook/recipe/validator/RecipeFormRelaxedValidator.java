package com.teenthofabud.restaurant.solution.cookbook.recipe.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineResponse;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.service.CuisineService;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
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
public class RecipeFormRelaxedValidator implements RelaxedValidator<RecipeForm>  {

    private List<String> fieldsToEscape;
    private CuisineService cuisineService;
    private CookbookServiceHelper cookbookServiceHelper;
    private Validator itemIdValidator;


    @Value("#{'${res.cookbook.recipe.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Autowired
    public void setCuisineService(CuisineService cuisineService) {
        this.cuisineService = cuisineService;
    }

    @Autowired
    @Qualifier("itemIdValidator")
    public void setItemIdValidator(Validator itemIdValidator) {
        this.itemIdValidator = itemIdValidator;
    }

    @Override
    public Boolean validateLoosely(RecipeForm form, Errors errors) {

        if(!fieldsToEscape.contains("name") && form.getName() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("RecipeForm.name is empty");
            errors.rejectValue("name", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return false;
        }
        log.debug("RecipeForm.name is valid");

        if(!fieldsToEscape.contains("instructions") && form.getInstructions() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getInstructions()))) {
            log.debug("RecipeForm.instructions is empty");
            errors.rejectValue("instructions", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return false;
        }
        log.debug("RecipeForm.instructions is valid");

        if(!fieldsToEscape.contains("cookingMethod") && form.getCookingMethod() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCookingMethod()))) {
            errors.rejectValue("cookingMethod", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.cookingMethod is invalid");
            return false;
        }
        log.debug("RecipeForm.cookingMethod is valid");

        if(!fieldsToEscape.contains("numberOfServings") && form.getNumberOfServings() != null && form.getNumberOfServings() <= 0) {
            errors.rejectValue("numberOfServings", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.numberOfServings is invalid");
            return false;
        }
        log.debug("RecipeForm.numberOfServings is valid");
        
        if(!fieldsToEscape.contains("preparationTimeDuration") && form.getPreparationTimeDuration() != null && form.getPreparationTimeDuration() <= 0.0d) {
            errors.rejectValue("preparationTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.preparationTimeDuration is invalid");
            return false;
        }
        log.debug("RecipeForm.preparationTimeDuration is valid");

        if(!fieldsToEscape.contains("preparationTimeUnitId") && form.getPreparationTimeUnitId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPreparationTimeUnitId()))) {
            errors.rejectValue("preparationTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.preparationTimeUnitId is empty");
            return false;
        } else if(!fieldsToEscape.contains("preparationTimeUnitId") && form.getPreparationTimeUnitId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getPreparationTimeUnitId()))) {
            if(!cookbookServiceHelper.isTimeIdValid(form.getPreparationTimeUnitId())) {
                log.debug("RecipeForm.preparationTimeUnitId is invalid");
                errors.rejectValue("preparationTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("RecipeForm.preparationTimeUnitId is valid");

        if(!fieldsToEscape.contains("cookingTimeDuration") && form.getCookingTimeDuration() != null && form.getCookingTimeDuration() <= 0.0d) {
            errors.rejectValue("cookingTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.cookingTimeDuration is invalid");
            return false;
        }
        log.debug("RecipeForm.cookingTimeDuration is valid");

        if(!fieldsToEscape.contains("cookingTimeUnitId") && form.getCookingTimeUnitId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCookingTimeUnitId()))) {
            errors.rejectValue("cookingTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.cookingTimeUnitId is empty");
            return false;
        } else if(!fieldsToEscape.contains("cookingTimeUnitId") && form.getCookingTimeUnitId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCookingTimeUnitId()))) {
            if(!cookbookServiceHelper.isTimeIdValid(form.getCookingTimeUnitId())) {
                log.debug("RecipeForm.cookingTimeUnitId is invalid");
                errors.rejectValue("cookingTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("RecipeForm.cookingTimeUnitId is valid");

        if(!fieldsToEscape.contains("portionSizeAmount") && form.getPortionSizeAmount() != null && form.getPortionSizeAmount() <= 0.0d) {
            errors.rejectValue("portionSizeAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.portionSizeAmount is invalid");
            return false;
        }
        log.debug("RecipeForm.portionSizeAmount is valid");

        if(!fieldsToEscape.contains("portionSizeUnitId") && form.getPortionSizeUnitId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPortionSizeUnitId()))) {
            errors.rejectValue("portionSizeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.portionSizeUnitId is empty");
            return false;
        } else if(!fieldsToEscape.contains("portionSizeUnitId") && form.getPortionSizeUnitId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getPortionSizeUnitId()))) {
            if(!cookbookServiceHelper.isWeightCodeValid(form.getPortionSizeUnitId())) {
                log.debug("RecipeForm.portionSizeUnitId is invalid");
                errors.rejectValue("portionSizeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("RecipeForm.portionSizeUnitId is valid");

        if(!fieldsToEscape.contains("itemId") && form.getItemId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getItemId()))) {
            log.debug("RecipeForm.itemId is empty");
            errors.rejectValue("itemId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("itemId") && form.getItemId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getItemId()))){
            Errors err = new DirectFieldBindingResult(form.getItemId(), "RecipeForm");
            itemIdValidator.validate(form.getItemId(), err);
            if(err.hasErrors()) {
                log.debug("RecipeForm.itemId is invalid");
                CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("RecipeForm error detail: {}", ec);
                errors.rejectValue("itemId", ec.name());
                return false;
            }
        }
        log.debug("RecipeForm.itemId is valid");

        if(!fieldsToEscape.contains("cuisineId") && form.getCuisineId() != null && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCuisineId()))) {
            log.debug("RecipeForm.cuisineId is empty");
            errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return false;
        } else if(!fieldsToEscape.contains("cuisineId") && form.getCuisineId() != null && StringUtils.hasText(StringUtils.trimWhitespace(form.getCuisineId()))){
            String cuisineId = form.getCuisineId();
            try {
                CuisineResponse cuisineResponse = cuisineService.retrieveDetailsById(cuisineId, Optional.of(TOABCascadeLevel.ONE));
                if(!cuisineResponse.getActive()) {
                    log.debug("RecipeForm.cuisineId is inactive");
                    errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    return false;
                }
            } catch (CuisineException e) {
                log.debug("RecipeForm.cuisineId is invalid");
                errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return false;
            }
        }
        log.debug("RecipeForm.cuisineId is valid");
        return true;
    }
}
