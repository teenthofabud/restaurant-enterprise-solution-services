package com.teenthofabud.restaurant.solution.inventory.quantity.converter;

import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.repository.ProductRepository;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityEntity;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class QuantityForm2EntityConverter implements Converter<QuantityForm, QuantityEntity> {

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
    public QuantityEntity convert(QuantityForm form) {
        QuantityEntity entity = new QuantityEntity();
        if(!fieldsToEscape.contains("amount")) {
            entity.setAmount(form.getAmount());
        }
        if(!fieldsToEscape.contains("weightId")) {
            entity.setWeightId(form.getWeightId());
        }
        if(!fieldsToEscape.contains("productId")) {
            Long productId = Long.parseLong(form.getProductId());
            Optional<ProductEntity> productEntity = productRepository.findById(productId);
            entity.setProduct(productEntity.get());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
