package com.teenthofabud.restaurant.solution.menu.item.converter;

import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemForm;
import com.teenthofabud.restaurant.solution.menu.item.data.VegeterianStatus;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ItemForm2EntityConverter implements Converter<ItemForm, ItemEntity> {

    private List<String> fieldsToEscape;
    private CategoryRepository categoryRepository;

    @Autowired
    public void setCategoryRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    @Value("#{'${res.menu.item.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public ItemEntity convert(ItemForm form) {
        ItemEntity entity = new ItemEntity();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        if(!fieldsToEscape.contains("isVegeterian")) {
            VegeterianStatus vegeterianStatus = VegeterianStatus.valueOf(form.getIsVegeterian());
            Boolean vegeterianSW = VegeterianStatus.getSwitchValue(vegeterianStatus);
            entity.setIsVegeterian(vegeterianSW);
        }
        if(!fieldsToEscape.contains("imageUrl")) {
            entity.setImageUrl(form.getImageUrl());
        }
        if(!fieldsToEscape.contains("categoryId")) {
            Long categoryId = Long.parseLong(form.getCategoryId());
            Optional<CategoryEntity> categoryEntity = categoryRepository.findById(categoryId);
            entity.setCategory(categoryEntity.get());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
