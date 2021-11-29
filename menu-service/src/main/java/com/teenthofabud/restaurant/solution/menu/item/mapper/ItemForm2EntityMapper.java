package com.teenthofabud.restaurant.solution.menu.item.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemForm;
import com.teenthofabud.restaurant.solution.menu.item.data.VegeterianStatus;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ItemForm2EntityMapper implements DualChannelMapper<ItemEntity, ItemForm> {

    private List<String> fieldsToEscape;
    private CategoryRepository categoryRepository;

    @Autowired
    public void setCategoryRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    @Value("#{'${res.menu.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<ItemEntity> compareAndMap(ItemEntity actualEntity, ItemForm form) {
        ItemEntity expectedEntity = new ItemEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying ItemEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying ItemEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying ItemEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("ItemForm.name: {} is different as ItemEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("ItemForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("ItemForm.description: {} is different as ItemEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("ItemForm.description: is unchanged");
        }

        if(!fieldsToEscape.contains("imageUrl") && StringUtils.hasText(StringUtils.trimWhitespace(form.getImageUrl()))
                && form.getImageUrl().compareTo(actualEntity.getImageUrl()) != 0) {
            expectedEntity.setImageUrl(form.getImageUrl());
            changeSW = true;
            log.debug("ItemForm.imageUrl: {} is different as ItemEntity.imageUrl: {}", form.getImageUrl(), actualEntity.getImageUrl());
        } else {
            expectedEntity.setImageUrl(actualEntity.getImageUrl());
            log.debug("ItemForm.imageUrl: is unchanged");
        }

        if(!fieldsToEscape.contains("categoryId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))) {
            Long categoryId = Long.parseLong(form.getCategoryId());
            Optional<CategoryEntity> optionalCategoryEntity = categoryRepository.findById(categoryId);
            if(actualEntity.getCategory().compareTo(optionalCategoryEntity.get()) != 0) {
                expectedEntity.setCategory(optionalCategoryEntity.get());
                changeSW = true;
                log.debug("ItemForm.categoryId: {} is different as ItemForm.categoryId: {}", form.getCategoryId(), actualEntity.getCategory().getId());
            } else {
                expectedEntity.setCategory(actualEntity.getCategory());
                log.debug("ItemForm.categoryId: is unchanged");
            }
        } else {
            expectedEntity.setCategory(actualEntity.getCategory());
            log.debug("ItemForm.categoryId: is unchanged");
        }

        if(!fieldsToEscape.contains("isVegeterian") && StringUtils.hasText(StringUtils.trimWhitespace(form.getIsVegeterian()))) {
            VegeterianStatus vegeterianStatus = VegeterianStatus.valueOf(form.getIsVegeterian());
            Boolean vegeterianSW = VegeterianStatus.getSwitchValue(vegeterianStatus);
            expectedEntity.setIsVegeterian(vegeterianSW);
            changeSW = true;
            log.debug("ItemForm.isVegeterian: {} is different as ItemEntity.isVegeterian: {}", form.getIsVegeterian(), actualEntity.getIsVegeterian());
        } else {
            expectedEntity.setIsVegeterian(actualEntity.getIsVegeterian());
            log.debug("ItemForm.isVegeterian: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
