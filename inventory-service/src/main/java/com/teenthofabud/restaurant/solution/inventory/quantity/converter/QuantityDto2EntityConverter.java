package com.teenthofabud.restaurant.solution.inventory.quantity.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.inventory.product.repository.ProductRepository;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityDto;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityEntity;
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
public class QuantityDto2EntityConverter implements ComparativePatchConverter<QuantityDto, QuantityEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 6;

    private List<String> fieldsToEscape;
    private ProductRepository productRepository;

    @Autowired
    public void setProductRepository(ProductRepository productRepository) {
        this.productRepository = productRepository;
    }
    
    @Value("#{'${res.inventory.quantity.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public void compareAndMap(QuantityDto dto, QuantityEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optAmount = dto.getAmount();
        if(!fieldsToEscape.contains("amount") && optAmount.isPresent()) {
            actualEntity.setAmount(Double.parseDouble(optAmount.get()));
            changeSW[i++] = true;
            log.debug("QuantityDto.amount is valid");
        }
        Optional<String> optProductId = dto.getProductId();
        if(!fieldsToEscape.contains("productId") && optProductId.isPresent()) {
            Long productId = Long.parseLong(optProductId.get());
            Optional<ProductEntity> productEntity = productRepository.findById(productId);
            actualEntity.setProduct(productEntity.get());
            changeSW[i++] = true;
            log.debug("QuantityDto.productId is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("QuantityDto.active is valid");
        }
        Optional<String> optWeightId = dto.getWeightId();
        if(!fieldsToEscape.contains("weightId") && optWeightId.isPresent()) {
            actualEntity.setWeightId(optWeightId.get());
            changeSW[i++] = true;
            log.debug("QuantityDto.weightId is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided QuantityDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided QuantityDto attributes are valid");
    }

}
