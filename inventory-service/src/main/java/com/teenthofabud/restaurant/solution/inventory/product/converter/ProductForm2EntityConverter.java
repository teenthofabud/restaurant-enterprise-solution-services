package com.teenthofabud.restaurant.solution.inventory.product.converter;

import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.inventory.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ProductForm2EntityConverter implements Converter<ProductForm, ProductEntity> {

    private List<String> fieldsToEscape;
    private CategoryRepository categoryRepository;

    @Autowired
    public void setCategoryRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    @Value("#{'${res.inventory.product.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public ProductEntity convert(ProductForm form) {
        ProductEntity entity = new ProductEntity();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
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
