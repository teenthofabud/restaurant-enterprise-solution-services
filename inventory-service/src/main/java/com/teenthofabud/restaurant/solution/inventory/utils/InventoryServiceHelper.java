package com.teenthofabud.restaurant.solution.inventory.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.inventory.category.converter.CategoryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.inventory.product.converter.ProductEntity2VoConverter;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.quantity.converter.QuantityEntity2VoConverter;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityEntity;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import tech.units.indriya.unit.BaseUnit;
import tech.units.indriya.unit.UnitDimension;
import tech.units.indriya.unit.Units;

import javax.measure.Unit;
import javax.measure.quantity.Mass;
import java.util.*;

@Slf4j
@Component
public class InventoryServiceHelper {

    private static final Unit<Mass> KG = new BaseUnit("kg", "Kilogram", UnitDimension.MASS);
    private static final Unit<Mass> G = new BaseUnit("g", "Gram", UnitDimension.MASS);

    private CategoryEntity2VoConverter categoryEntity2VoConverter;
    private ProductEntity2VoConverter productEntity2VoConverter;
    private QuantityEntity2VoConverter quantityEntity2VoConverter;

    @Autowired
    public void setQuantityEntity2VoConverter(QuantityEntity2VoConverter quantityEntity2VoConverter) {
        this.quantityEntity2VoConverter = quantityEntity2VoConverter;
    }

    @Autowired
    public void setCategoryEntity2VoConverter(CategoryEntity2VoConverter categoryEntity2VoConverter) {
        this.categoryEntity2VoConverter = categoryEntity2VoConverter;
    }

    @Autowired
    public void setProductEntity2VoConverter(ProductEntity2VoConverter productEntity2VoConverter) {
        this.productEntity2VoConverter = productEntity2VoConverter;
    }

    public List<CategoryVo> categoryEntity2DetailedVo(List<? extends CategoryEntity> categoryEntityList) {
        List<CategoryVo> categoryDetailsList = new LinkedList<>();
        if(categoryEntityList != null && !categoryEntityList.isEmpty()) {
            for(CategoryEntity entity : categoryEntityList) {
                CategoryVo vo = this.categoryEntity2DetailedVo(entity);
                categoryDetailsList.add(vo);
            }
        }
        return categoryDetailsList;
    }

    public List<ProductVo> productEntity2DetailedVo(List<? extends ProductEntity> productEntityList) {
        List<ProductVo> productDetailsList = new LinkedList<>();
        if(productEntityList != null && !productEntityList.isEmpty()) {
            for(ProductEntity entity : productEntityList) {
                ProductVo vo = this.productEntity2DetailedVo(entity);
                productDetailsList.add(vo);
            }
        }
        return productDetailsList;
    }

    public CategoryVo categoryEntity2DetailedVo(CategoryEntity categoryEntity) {
        if(categoryEntity != null) {
            CategoryVo vo = categoryEntity2VoConverter.convert(categoryEntity);
            log.debug("Converting {} to {}", categoryEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "category entity is null" });
    }

    public ProductVo productEntity2DetailedVo(ProductEntity productEntity) {
        if(productEntity != null) {
            ProductVo vo = productEntity2VoConverter.convert(productEntity);
            log.debug("Converting {} to {}", productEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "product entity is null" });
    }

    public boolean isWeightCodeValid(String weightCode) {
        Optional<Unit<Mass>> optionalWeight = this.parseWeightCode(weightCode);
        return optionalWeight.isPresent();
    }

    public Optional<Unit<Mass>> parseWeightCode(String weightCode) {
        /*Set<Unit<?>> units = Units.getInstance().getUnits();
        if(units == null || units.isEmpty()) {
            log.debug("no units available in the system");
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object [] { "no units available in the system" });
        }*/
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(weightCode))) {
            log.debug("weight code is empty");
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object [] { "weight code is empty" });
        }
        Optional<Unit<Mass>> optionalWeight = KG.getSymbol().compareTo(weightCode.toLowerCase()) == 0 ? Optional.of(KG)
                : G.getSymbol().compareTo(weightCode.toLowerCase()) == 0 ? Optional.of(G) :Optional.empty();
        /*optionalWeight = units.stream().filter(c -> (c.isCompatible(KG)
                && c.getSymbol().compareTo(weightCode.toLowerCase()) == 0)).findAny();*/
        return optionalWeight;
    }

    public List<QuantityVo> quantityEntity2DetailedVo(List<QuantityEntity> quantityEntityList) {
        List<QuantityVo> quantityDetailsList = new LinkedList<>();
        if(quantityEntityList != null && !quantityEntityList.isEmpty()) {
            for(QuantityEntity entity : quantityEntityList) {
                QuantityVo vo = this.quantityEntity2DetailedVo(entity);
                quantityDetailsList.add(vo);
            }
        }
        return quantityDetailsList;
    }

    public QuantityVo quantityEntity2DetailedVo(QuantityEntity quantityEntity) {
        if(quantityEntity != null) {
            QuantityVo vo = quantityEntity2VoConverter.convert(quantityEntity);
            log.debug("Converting {} to {}", quantityEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "quantity entity is null" });
    }
}
