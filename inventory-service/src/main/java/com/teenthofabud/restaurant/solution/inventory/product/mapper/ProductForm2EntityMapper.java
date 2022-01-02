package com.teenthofabud.restaurant.solution.inventory.product.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.inventory.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ProductForm2EntityMapper implements DualChannelMapper<ProductEntity, ProductForm> {

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
    public Optional<ProductEntity> compareAndMap(ProductEntity actualEntity, ProductForm form) {
        ProductEntity expectedEntity = new ProductEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying ProductEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying ProductEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying ProductEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualEntity.getName()) != 0) {
            expectedEntity.setName(form.getName());
            changeSW = true;
            log.debug("ProductForm.name: {} is different as ProductEntity.name: {}", form.getName(), actualEntity.getName());
        } else {
            expectedEntity.setName(actualEntity.getName());
            log.debug("ProductForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualEntity.getDescription()) != 0) {
            expectedEntity.setDescription(form.getDescription());
            changeSW = true;
            log.debug("ProductForm.description: {} is different as ProductEntity.description: {}", form.getDescription(), actualEntity.getDescription());
        } else {
            expectedEntity.setDescription(actualEntity.getDescription());
            log.debug("ProductForm.description: is unchanged");
        }

        if(!fieldsToEscape.contains("imageUrl") && StringUtils.hasText(StringUtils.trimWhitespace(form.getImageUrl()))
                && form.getImageUrl().compareTo(actualEntity.getImageUrl()) != 0) {
            expectedEntity.setImageUrl(form.getImageUrl());
            changeSW = true;
            log.debug("ProductForm.imageUrl: {} is different as ProductEntity.imageUrl: {}", form.getImageUrl(), actualEntity.getImageUrl());
        } else {
            expectedEntity.setImageUrl(actualEntity.getImageUrl());
            log.debug("ProductForm.imageUrl: is unchanged");
        }

        if(!fieldsToEscape.contains("categoryId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))) {
            Long categoryId = Long.parseLong(form.getCategoryId());
            Optional<CategoryEntity> optionalCategoryEntity = categoryRepository.findById(categoryId);
            if(actualEntity.getCategory().compareTo(optionalCategoryEntity.get()) != 0) {
                expectedEntity.setCategory(optionalCategoryEntity.get());
                changeSW = true;
                log.debug("ProductForm.categoryId: {} is different as ProductForm.categoryId: {}", form.getCategoryId(), actualEntity.getCategory().getId());
            } else {
                expectedEntity.setCategory(actualEntity.getCategory());
                log.debug("ProductForm.categoryId: is unchanged");
            }
        } else {
            expectedEntity.setCategory(actualEntity.getCategory());
            log.debug("ProductForm.categoryId: is unchanged");
        }

        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
