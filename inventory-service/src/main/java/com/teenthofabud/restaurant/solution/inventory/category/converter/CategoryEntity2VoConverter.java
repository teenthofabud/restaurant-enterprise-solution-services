package com.teenthofabud.restaurant.solution.inventory.category.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.utils.InventoryServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.*;

@Component
@Slf4j
public class CategoryEntity2VoConverter extends TOABBaseEntity2VoConverter<CategoryEntity, CategoryVo> implements Converter<CategoryEntity, CategoryVo> {

    private List<String> fieldsToEscape;
    private InventoryServiceHelper inventoryServiceHelper;

    @Value("#{'${res.inventory.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }


    @Autowired
    public void setInventoryServiceHelper(InventoryServiceHelper inventoryServiceHelper) {
        this.inventoryServiceHelper = inventoryServiceHelper;
    }

    @Override
    public CategoryVo convert(CategoryEntity entity) {
        CategoryVo vo = new CategoryVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("items")) {
            this.expandSecondLevelFields(entity, vo, "items");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(CategoryEntity entity, CategoryVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("items") && fieldName.compareTo("items") == 0) {
                    Callable<List<ProductVo>> itemEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        List<ProductEntity> itemEntities = entity.getProducts();
                        List<ProductVo> itemDetailsList = inventoryServiceHelper.productEntity2DetailedVo(itemEntities);
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return itemDetailsList;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("itemEntity2VoConversion-"));
                    Future<List<ProductVo>> itemEntity2VoConversionResult = executorService.submit(itemEntity2VoConversion);
                    try {
                        List<ProductVo> itemDetailsList = itemEntity2VoConversionResult.get();
                        vo.setProducts(itemDetailsList);
                        log.debug("Retrieved {} items for account id: {}", itemDetailsList.size(), entity.getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform itemEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform itemEntity2VoConversion",
                                new Object[] { "itemEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                break;
            default:
                log.debug("No fields available for cascading");
                break;
        }
    }

}
