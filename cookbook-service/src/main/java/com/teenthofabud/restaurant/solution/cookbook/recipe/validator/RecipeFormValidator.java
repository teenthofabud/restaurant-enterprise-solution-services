package com.teenthofabud.restaurant.solution.cookbook.recipe.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineVo;
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
public class RecipeFormValidator implements Validator {

    private List<String> fieldsToEscape;
    private CookbookServiceHelper cookbookServiceHelper;
    private CuisineService cuisineService;
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
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(RecipeForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        RecipeForm form = (RecipeForm) target;

        if(!fieldsToEscape.contains("name") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("RecipeForm.name is empty");
            errors.rejectValue("name", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("RecipeForm.name is valid");

        if(!fieldsToEscape.contains("instructions") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getInstructions()))) {
            log.debug("RecipeForm.instructions is empty");
            errors.rejectValue("instructions", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        }
        log.debug("RecipeForm.instructions is valid");

        if(!fieldsToEscape.contains("cookingMethod") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCookingMethod()))) {
            errors.rejectValue("cookingMethod", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.cookingMethod is invalid");
            return;
        }
        log.debug("RecipeForm.cookingMethod is valid");

        if(!fieldsToEscape.contains("numberOfServings") && form.getNumberOfServings() == null) {
            errors.rejectValue("numberOfServings", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.numberOfServings is empty");
            return;
        } else if(!fieldsToEscape.contains("numberOfServings") && form.getNumberOfServings() != null && form.getNumberOfServings() <= 0.0d) {
            errors.rejectValue("numberOfServings", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.numberOfServings is invalid");
            return;
        }
        log.debug("RecipeForm.numberOfServings is valid");

        if(!fieldsToEscape.contains("preparationTimeDuration") && form.getPreparationTimeDuration() == null) {
            errors.rejectValue("preparationTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.preparationTimeDuration is empty");
            return;
        } else if(!fieldsToEscape.contains("preparationTimeDuration") && form.getPreparationTimeDuration() != null && form.getPreparationTimeDuration() <= 0.0d) {
            errors.rejectValue("preparationTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.preparationTimeDuration is invalid");
            return;
        }
        log.debug("RecipeForm.preparationTimeDuration is valid");

        if(!fieldsToEscape.contains("preparationTimeUnitId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPreparationTimeUnitId()))) {
            errors.rejectValue("preparationTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.preparationTimeUnitId is empty");
            return;
        } else if(!fieldsToEscape.contains("preparationTimeUnitId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPreparationTimeUnitId()))) {
            if(!cookbookServiceHelper.isTimeIdValid(form.getPreparationTimeUnitId())) {
                log.debug("RecipeForm.preparationTimeUnitId is invalid");
                errors.rejectValue("preparationTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("RecipeForm.preparationTimeUnitId is valid");

        if(!fieldsToEscape.contains("cookingTimeDuration") && form.getCookingTimeDuration() == null) {
            errors.rejectValue("cookingTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.cookingTimeDuration is empty");
            return;
        } else if(!fieldsToEscape.contains("cookingTimeDuration") && form.getCookingTimeDuration() != null && form.getCookingTimeDuration() <= 0.0d) {
            errors.rejectValue("cookingTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.cookingTimeDuration is invalid");
            return;
        }
        log.debug("RecipeForm.cookingTimeDuration is valid");

        if(!fieldsToEscape.contains("cookingTimeUnitId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCookingTimeUnitId()))) {
            errors.rejectValue("cookingTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.cookingTimeUnitId is empty");
            return;
        } else if(!fieldsToEscape.contains("cookingTimeUnitId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCookingTimeUnitId()))) {
            if(!cookbookServiceHelper.isTimeIdValid(form.getCookingTimeUnitId())) {
                log.debug("RecipeForm.cookingTimeUnitId is invalid");
                errors.rejectValue("cookingTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("RecipeForm.cookingTimeUnitId is valid");

        if(!fieldsToEscape.contains("portionSizeAmount") && form.getPortionSizeAmount() == null) {
            errors.rejectValue("portionSizeAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.portionSizeAmount is empty");
            return;
        } else if(!fieldsToEscape.contains("portionSizeAmount") && form.getPortionSizeAmount() != null && form.getPortionSizeAmount() <= 0.0d) {
            errors.rejectValue("portionSizeAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.portionSizeAmount is invalid");
            return;
        }
        log.debug("RecipeForm.portionSizeAmount is valid");

        if(!fieldsToEscape.contains("portionSizeUnitId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getPortionSizeUnitId()))) {
            errors.rejectValue("portionSizeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeForm.portionSizeUnitId is empty");
            return;
        } else if(!fieldsToEscape.contains("portionSizeUnitId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getPortionSizeUnitId()))) {
            if(!cookbookServiceHelper.isWeightCodeValid(form.getPortionSizeUnitId())) {
                log.debug("RecipeForm.portionSizeUnitId is invalid");
                errors.rejectValue("portionSizeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("RecipeForm.portionSizeUnitId is valid");

        if(!fieldsToEscape.contains("itemId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getItemId()))) {
            log.debug("RecipeForm.itemId is empty");
            errors.rejectValue("itemId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("itemId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getItemId()))){
            Errors err = new DirectFieldBindingResult(form.getItemId(), "RecipeForm");
            itemIdValidator.validate(form.getItemId(), err);
            if(err.hasErrors()) {
                log.debug("RecipeForm.itemId is invalid");
                CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("RecipeForm error detail: {}", ec);
                errors.rejectValue("itemId", ec.name());
                return;
            }
        }
        log.debug("RecipeForm.itemId is valid");

        if(!fieldsToEscape.contains("cuisineId") && StringUtils.isEmpty(StringUtils.trimWhitespace(form.getCuisineId()))) {
            log.debug("RecipeForm.cuisineId is empty");
            errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            return;
        } else if(!fieldsToEscape.contains("cuisineId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCuisineId()))){
            String cuisineId = form.getCuisineId();
            try {
                CuisineVo cuisineVo = cuisineService.retrieveDetailsById(cuisineId, Optional.of(TOABCascadeLevel.ONE));
                if(!cuisineVo.getActive()) {
                    log.debug("RecipeForm.cuisineId is inactive");
                    errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (CuisineException e) {
                log.debug("RecipeForm.cuisineId is invalid");
                errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        log.debug("RecipeForm.cuisineId is valid");
    }

}
