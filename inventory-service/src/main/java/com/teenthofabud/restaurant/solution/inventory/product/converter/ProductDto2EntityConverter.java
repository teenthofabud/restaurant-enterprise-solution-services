package com.teenthofabud.restaurant.solution.inventory.product.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.inventory.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductDto;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
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
public class ProductDto2EntityConverter implements ComparativePatchConverter<ProductDto, ProductEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 5;

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
    public void compareAndMap(ProductDto dto, ProductEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optName = dto.getName();
        if(!fieldsToEscape.contains("name") && optName.isPresent()) {
            actualEntity.setName(optName.get());
            changeSW[i++] = true;
            log.debug("ProductDto.name is valid");
        }
        Optional<String> optDescription = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDescription.isPresent()) {
            actualEntity.setDescription(optDescription.get());
            changeSW[i++] = true;
            log.debug("ProductDto.description is valid");
        }
        Optional<String> optImageUrl = dto.getImageUrl();
        if(!fieldsToEscape.contains("imageUrl") && optImageUrl.isPresent()) {
            actualEntity.setDescription(optImageUrl.get());
            changeSW[i++] = true;
            log.debug("ProductDto.imageUrl is valid");
        }
        Optional<String> optCategoryId = dto.getCategoryId();
        if(!fieldsToEscape.contains("categoryId") && optCategoryId.isPresent()) {
            Long categoryId = Long.parseLong(optCategoryId.get());
            Optional<CategoryEntity> categoryEntity = categoryRepository.findById(categoryId);
            actualEntity.setCategory(categoryEntity.get());
            changeSW[i++] = true;
            log.debug("ProductDto.categoryId is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("ProductDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided ProductDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided ProductDto attributes are valid");
    }

}
