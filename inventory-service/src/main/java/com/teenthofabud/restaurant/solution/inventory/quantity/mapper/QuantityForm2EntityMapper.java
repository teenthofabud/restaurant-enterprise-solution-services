package com.teenthofabud.restaurant.solution.inventory.quantity.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.repository.ProductRepository;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityEntity;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class QuantityForm2EntityMapper implements DualChannelMapper<QuantityEntity, QuantityForm> {

    private List<String> fieldsToEscape;
    private ProductRepository productRepository;

    @Autowired
    public void setProductRepository(ProductRepository productRepository) {
        this.productRepository = productRepository;
    }

    @Value("#{'${res.inventory.product.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<QuantityEntity> compareAndMap(QuantityEntity actualEntity, QuantityForm form) {
        QuantityEntity expectedEntity = new QuantityEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying QuantityEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying QuantityEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying QuantityEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("amount") && form.getAmount() != null && form.getAmount().compareTo(actualEntity.getAmount()) != 0) {
            expectedEntity.setAmount(form.getAmount());
            changeSW = true;
            log.debug("QuantityForm.name: {} is different as QuantityEntity.name: {}", form.getAmount(), actualEntity.getAmount());
        } else {
            expectedEntity.setAmount(actualEntity.getAmount());
            log.debug("QuantityForm.name: is unchanged");
        }

        if(!fieldsToEscape.contains("productId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getProductId()))
                && form.getProductId().compareTo(actualEntity.getProduct().getId().toString()) != 0) {
            Long productId = Long.parseLong(form.getProductId());
            Optional<ProductEntity> optionalProductEntity = productRepository.findById(productId);
            if(actualEntity.getProduct().compareTo(optionalProductEntity.get()) != 0) {
                expectedEntity.setProduct(optionalProductEntity.get());
                changeSW = true;
                log.debug("QuantityForm.productId: {} is different as QuantityForm.productId: {}", form.getProductId(), actualEntity.getProduct().getId());
            } else {
                expectedEntity.setProduct(actualEntity.getProduct());
                log.debug("QuantityForm.productId: is unchanged");
            }
        } else {
            expectedEntity.setProduct(actualEntity.getProduct());
            log.debug("QuantityForm.productId: is unchanged");
        }

        if(!fieldsToEscape.contains("weightId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getWeightId()))
                && form.getWeightId().compareTo(actualEntity.getWeightId()) != 0) {
            expectedEntity.setWeightId(form.getWeightId());
            changeSW = true;
            log.debug("QuantityForm.weightId: {} is different as QuantityEntity.weightId: {}", form.getWeightId(), actualEntity.getWeightId());
        } else {
            expectedEntity.setWeightId(actualEntity.getWeightId());
            log.debug("QuantityForm.weightId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }

}
