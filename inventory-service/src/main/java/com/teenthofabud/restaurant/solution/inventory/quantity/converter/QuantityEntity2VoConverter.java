package com.teenthofabud.restaurant.solution.inventory.quantity.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityEntity;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityVo;
import com.teenthofabud.restaurant.solution.inventory.utils.InventoryServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import javax.measure.Unit;
import javax.measure.quantity.Mass;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;

@Component
@Slf4j
public class QuantityEntity2VoConverter extends TOABBaseEntity2VoConverter<QuantityEntity, QuantityVo> implements Converter<QuantityEntity, QuantityVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.inventory.quantity.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private InventoryServiceHelper inventoryServiceHelper;

    @Autowired
    public void setInventoryServiceHelper(InventoryServiceHelper inventoryServiceHelper) {
        this.inventoryServiceHelper = inventoryServiceHelper;
    }

    @Override
    public QuantityVo convert(QuantityEntity entity) {
        QuantityVo vo = new QuantityVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("amount")) {
            vo.setAmount(entity.getAmount());
        }
        if(!fieldsToEscape.contains("weightId")) {
            expandSecondLevelFields(entity, vo, "weightId");
        }
        if(!fieldsToEscape.contains("productId")) {
            expandSecondLevelFields(entity, vo, "productId");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(QuantityEntity entity, QuantityVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("productId") && fieldName.compareTo("productId") == 0) {
                    Callable<ProductVo> productEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        ProductVo ProductVo = inventoryServiceHelper.productEntity2DetailedVo(entity.getProduct());
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return ProductVo;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("productEntity2VoConversion-"));
                    Future<ProductVo> productEntity2VoConversionResult = executorService.submit(productEntity2VoConversion);
                    try {
                        ProductVo ProductVo = productEntity2VoConversionResult.get();
                        vo.setProduct(ProductVo);
                        log.debug("Retrieved {} for ProductId: {}", ProductVo, entity.getProduct().getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform productEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform productEntity2VoConversion",
                                new Object[] { "productEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("weightId") && fieldName.compareTo("weightId") == 0) {
                    if(inventoryServiceHelper.isWeightCodeValid(entity.getWeightId())) {
                        Optional<Unit<Mass>> optionalWeight = inventoryServiceHelper.parseWeightCode(entity.getWeightId());
                        vo.setWeight(optionalWeight.get());
                    } else {
                        log.error("Unable to parse weight id");
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to parse weight id", new Object[]{"Unable to parse weight id"});
                    }
                }
                break;
            default:
                vo.setProductId(entity.getProduct().getId().toString());
                vo.setWeightId(entity.getWeightId());
                log.debug("only first level cascaded for ProductId");
                break;
        }
    }

}
