package com.teenthofabud.restaurant.solution.cookbook.recipe.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.repository.CuisineJPARepository;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeDto;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
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
public class RecipeDto2EntityConverter implements ComparativePatchConverter<RecipeDto, RecipeEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 13;

    private List<String> fieldsToEscape;
    private CookbookServiceHelper cookbookServiceHelper;

    @Value("#{'${res.cookbook.recipe.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private CuisineJPARepository cuisineRepository;

    @Autowired
    public void setCuisineRepository(CuisineJPARepository cuisineRepository) {
        this.cuisineRepository = cuisineRepository;
    }

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Override
    public void compareAndMap(RecipeDto dto, RecipeEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optCuisineId = dto.getCuisineId();
        if(!fieldsToEscape.contains("cuisineId") && optCuisineId.isPresent()) {
            Long cuisineId = Long.parseLong(optCuisineId.get());
            Optional<CuisineEntity> optCuisineEntity = cuisineRepository.findById(cuisineId);
            actualEntity.setCuisine(optCuisineEntity.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.cuisineId is valid");
        }
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.name is valid");
        }
        Optional<String> optCookingMethod = dto.getCookingMethod();
        if(!fieldsToEscape.contains("cookingMethod") && optCookingMethod.isPresent()) {
            actualEntity.setCookingMethod(optCookingMethod.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.cookingMethod is valid");
        }
        Optional<String> optInstructions = dto.getInstructions();
        if(!fieldsToEscape.contains("instructions") && optInstructions.isPresent()) {
            actualEntity.setInstructions(optInstructions.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.instructions is valid");
        }
        Optional<String> optItemId = dto.getItemId();
        if(!fieldsToEscape.contains("itemId") && optItemId.isPresent()) {
            actualEntity.setItemId(optItemId.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.itemId is valid");
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.description is valid");
        }
        Optional<String> optNumberOfServings = dto.getNumberOfServings();
        if(!fieldsToEscape.contains("numberOfServings") && optNumberOfServings.isPresent()) {
            Integer numberOfServings = Integer.parseInt(optNumberOfServings.get());
            actualEntity.setNumberOfServings(numberOfServings);
            changeSW[i++] = true;
            log.debug("RecipeDto.numberOfServings is valid");
        }
        Optional<String> optPreparationTimeDuration = dto.getPreparationTimeDuration();
        if(!fieldsToEscape.contains("preparationTimeDuration") && optPreparationTimeDuration.isPresent()) {
            Double preparationTimeDuration = Double.parseDouble(optPreparationTimeDuration.get());
            actualEntity.setPreparationTimeDuration(preparationTimeDuration);
            changeSW[i++] = true;
            log.debug("RecipeDto.preparationTimeDuration is valid");
        }
        Optional<String> optPreparationTimeUnitId = dto.getPreparationTimeUnitId();
        if(!fieldsToEscape.contains("preparationTimeUnitId") && optPreparationTimeUnitId.isPresent()) {
            actualEntity.setPreparationTimeUnitId(optPreparationTimeUnitId.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.preparationTimeUnitId is valid");
        }
        Optional<String> optCookingTimeDuration = dto.getCookingTimeDuration();
        if(!fieldsToEscape.contains("cookingTimeDuration") && optCookingTimeDuration.isPresent()) {
            Double cookingTimeDuration = Double.parseDouble(optCookingTimeDuration.get());
            actualEntity.setCookingTimeDuration(cookingTimeDuration);
            changeSW[i++] = true;
            log.debug("RecipeDto.cookingTimeDuration is valid");
        }
        Optional<String> optCookingTimeUnitId = dto.getCookingTimeUnitId();
        if(!fieldsToEscape.contains("cookingTimeUnitId") && optCookingTimeUnitId.isPresent()) {
            actualEntity.setCookingTimeUnitId(optCookingTimeUnitId.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.cookingTimeUnitId is valid");
        }
        Optional<String> optPortionSizeAmount = dto.getPortionSizeAmount();
        if(!fieldsToEscape.contains("portionSizeAmount") && optPortionSizeAmount.isPresent()) {
            Double portionSizeAmount = Double.parseDouble(optPortionSizeAmount.get());
            actualEntity.setPortionSizeAmount(portionSizeAmount);
            changeSW[i++] = true;
            log.debug("RecipeDto.portionSizeAmount is valid");
        }
        Optional<String> optPortionSizeUnitId = dto.getPortionSizeUnitId();
        if(!fieldsToEscape.contains("portionSizeUnitId") && optPortionSizeUnitId.isPresent()) {
            actualEntity.setPortionSizeUnitId(optPortionSizeUnitId.get());
            changeSW[i++] = true;
            log.debug("RecipeDto.portionSizeUnitId is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("RecipeDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided RecipeDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided RecipeDto attributes are valid");
    }

}
