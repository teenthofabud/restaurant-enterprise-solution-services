package com.teenthofabud.restaurant.solution.cookbook.recipe.validator;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineResponse;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.service.CuisineService;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeDto;
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
public class RecipeDtoValidator implements Validator {

    private List<String> fieldsToEscape;
    private Validator itemIdValidator;
    private CookbookServiceHelper cookbookServiceHelper;
    private CuisineService cuisineService;

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
        return clazz.isAssignableFrom(RecipeDto.class);
    }

    /**
     * Order in which the fields are being validated are important
     */

    @Override
    public void validate(Object target, Errors errors) {
        RecipeDto dto = (RecipeDto) target;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.name is invalid");
        }

        Optional<String> optInstructions = dto.getInstructions();
        if(!fieldsToEscape.contains("instructions") && optInstructions.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optInstructions.get()))) {
            errors.rejectValue("instructions", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.instructions is invalid");
            return;
        }

        Optional<String> optCookingMethod = dto.getCookingMethod();
        if(!fieldsToEscape.contains("cookingMethod") && optCookingMethod.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCookingMethod.get()))) {
            errors.rejectValue("cookingMethod", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.cookingMethod is invalid");
            return;
        }

        Optional<String> optNumberOfServings = dto.getNumberOfServings();
        if(!fieldsToEscape.contains("numberOfServings") && optNumberOfServings.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optNumberOfServings.get()))) {
            errors.rejectValue("numberOfServings", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.numberOfServings is empty");
            return;
        } else if(!fieldsToEscape.contains("numberOfServings") && optNumberOfServings.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optNumberOfServings.get()))) {
            try {
                Integer numberOfServings = Integer.parseInt(optNumberOfServings.get());
                if(numberOfServings <= 0) {
                    errors.rejectValue("numberOfServings", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    log.debug("RecipeDto.numberOfServings is invalid");
                    return;
                }
            } catch (NumberFormatException e) {
                errors.rejectValue("numberOfServings", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                log.debug("RecipeDto.numberOfServings is invalid");
                return;
            }
        }

        Optional<String> optPreparationTimeDuration = dto.getPreparationTimeDuration();
        if(!fieldsToEscape.contains("preparationTimeDuration") && optPreparationTimeDuration.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optPreparationTimeDuration.get()))) {
            errors.rejectValue("preparationTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.preparationTimeDuration is empty");
            return;
        } else if(!fieldsToEscape.contains("preparationTimeDuration") && optPreparationTimeDuration.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optPreparationTimeDuration.get()))) {
            try {
                Double duration = Double.parseDouble(optPreparationTimeDuration.get());
                if(duration <= 0.0d) {
                    errors.rejectValue("preparationTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    log.debug("RecipeDto.preparationTimeDuration is invalid");
                    return;
                }
            } catch (NumberFormatException e) {
                errors.rejectValue("preparationTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                log.debug("RecipeDto.preparationTimeDuration is invalid");
                return;
            }
        }

        Optional<String> optPreparationTimeUnitId = dto.getPreparationTimeUnitId();
        if(!fieldsToEscape.contains("preparationTimeUnitId") && optPreparationTimeUnitId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optPreparationTimeUnitId.get()))) {
            errors.rejectValue("preparationTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.preparationTimeUnitId is empty");
            return;
        } else if(!fieldsToEscape.contains("preparationTimeUnitId") && optPreparationTimeUnitId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optPreparationTimeUnitId.get()))) {
            if(!cookbookServiceHelper.isTimeIdValid(optPreparationTimeUnitId.get())) {
                log.debug("RecipeDto.preparationTimeUnitId is invalid");
                errors.rejectValue("preparationTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optCookingTimeDuration = dto.getCookingTimeDuration();
        if(!fieldsToEscape.contains("cookingTimeDuration") && optCookingTimeDuration.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCookingTimeDuration.get()))) {
            errors.rejectValue("cookingTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.cookingTimeDuration is empty");
            return;
        } else if(!fieldsToEscape.contains("cookingTimeDuration") && optCookingTimeDuration.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCookingTimeDuration.get()))) {
            try {
                Double duration = Double.parseDouble(optCookingTimeDuration.get());
                if(duration <= 0.0d) {
                    errors.rejectValue("cookingTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    log.debug("RecipeDto.cookingTimeDuration is invalid");
                    return;
                }
            } catch (NumberFormatException e) {
                errors.rejectValue("cookingTimeDuration", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                log.debug("RecipeDto.cookingTimeDuration is invalid");
                return;
            }
        }

        Optional<String> optCookingTimeUnitId = dto.getCookingTimeUnitId();
        if(!fieldsToEscape.contains("cookingTimeUnitId") && optCookingTimeUnitId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCookingTimeUnitId.get()))) {
            errors.rejectValue("cookingTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.cookingTimeUnitId is empty");
            return;
        } else if(!fieldsToEscape.contains("cookingTimeUnitId") && optCookingTimeUnitId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCookingTimeUnitId.get()))) {
            if(!cookbookServiceHelper.isTimeIdValid(optCookingTimeUnitId.get())) {
                log.debug("RecipeDto.cookingTimeUnitId is invalid");
                errors.rejectValue("cookingTimeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optPortionSizeAmount = dto.getPortionSizeAmount();
        if(!fieldsToEscape.contains("portionSizeAmount") && optPortionSizeAmount.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optPortionSizeAmount.get()))) {
            errors.rejectValue("portionSizeAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.portionSizeAmount is empty");
            return;
        } else if(!fieldsToEscape.contains("portionSizeAmount") && optPortionSizeAmount.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optPortionSizeAmount.get()))) {
            try {
                Double duration = Double.parseDouble(optPortionSizeAmount.get());
                if(duration <= 0.0d) {
                    errors.rejectValue("portionSizeAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    log.debug("RecipeDto.portionSizeAmount is invalid");
                    return;
                }
            } catch (NumberFormatException e) {
                errors.rejectValue("portionSizeAmount", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                log.debug("RecipeDto.portionSizeAmount is invalid");
                return;
            }
        }

        Optional<String> optPortionSizeUnitId = dto.getPortionSizeUnitId();
        if(!fieldsToEscape.contains("portionSizeUnitId") && optPortionSizeUnitId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optPortionSizeUnitId.get()))) {
            errors.rejectValue("portionSizeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.portionSizeUnitId is empty");
            return;
        } else if(!fieldsToEscape.contains("portionSizeUnitId") && optPortionSizeUnitId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optPortionSizeUnitId.get()))) {
            if(!cookbookServiceHelper.isWeightCodeValid(optPortionSizeUnitId.get())) {
                log.debug("RecipeDto.portionSizeUnitId is invalid");
                errors.rejectValue("portionSizeUnitId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optItemId = dto.getItemId();
        if(!fieldsToEscape.contains("itemId") && optItemId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optItemId.get()))) {
            errors.rejectValue("itemId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.itemId is invalid");
            return;
        } else if(!fieldsToEscape.contains("itemId") && optItemId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optItemId.get()))) {
            String itemId = optItemId.get();
            Errors err = new DirectFieldBindingResult(itemId, "RecipeDto");
            itemIdValidator.validate(itemId, err);
            if(err.hasErrors()) {
                log.debug("RecipeDto.itemId is invalid");
                CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getGlobalError().getCode());
                log.debug("RecipeDto error detail: {}", ec);
                errors.rejectValue("itemId", ec.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                log.debug("RecipeDto.active is invalid");
                return;
            }
        }

        Optional<String> optCuisineId = dto.getCuisineId();
        if(!fieldsToEscape.contains("cuisineId") && optCuisineId.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optCuisineId.get()))) {
            errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
            log.debug("RecipeDto.cuisineId is invalid");
            return;
        } else if(!fieldsToEscape.contains("cuisineId") && optCuisineId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optCuisineId.get()))) {
            String cuisineId = optCuisineId.get();
            try {
                Long.parseLong(cuisineId);
            } catch (NumberFormatException e) {
                log.debug("RecipeDto.cuisineId is invalid");
                errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
            try {
                CuisineResponse cuisineResponse = cuisineService.retrieveDetailsById(cuisineId, Optional.of(TOABCascadeLevel.ONE));
                if(!cuisineResponse.getActive()) {
                    log.debug("RecipeDto.cuisineId is inactive");
                    errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                    return;
                }
            } catch (CuisineException e) {
                log.debug("RecipeDto.cuisineId is invalid");
                errors.rejectValue("cuisineId", CookbookErrorCode.COOK_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }

}
