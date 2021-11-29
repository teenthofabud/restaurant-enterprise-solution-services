package com.teenthofabud.restaurant.solution.menu.item.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseEntity2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.item.data.VegeterianStatus;
import com.teenthofabud.restaurant.solution.menu.utils.MenuServiceHelper;
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
public class ItemEntity2VoConverter extends TOABBaseEntity2VoConverter<ItemEntity, ItemVo> implements Converter<ItemEntity, ItemVo> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.menu.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private MenuServiceHelper menuServiceHelper;

    @Autowired
    public void setMenuServiceHelper(MenuServiceHelper menuServiceHelper) {
        this.menuServiceHelper = menuServiceHelper;
    }

    @Override
    public ItemVo convert(ItemEntity entity) {
        ItemVo vo = new ItemVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(entity.getId().toString());
        }
        if(!fieldsToEscape.contains("name")) {
            vo.setName(entity.getName());
        }
        if(!fieldsToEscape.contains("description")) {
            vo.setDescription(entity.getDescription());
        }
        if(!fieldsToEscape.contains("isVegeterian")) {
            VegeterianStatus vegeterianStatus = VegeterianStatus.getSwitchValue(entity.getIsVegeterian());
            vo.setIsVegeterian(vegeterianStatus);
        }
        if(!fieldsToEscape.contains("imageUrl")) {
            vo.setImageUrl(entity.getImageUrl());
        }
        if(!fieldsToEscape.contains("categoryId")) {
            expandSecondLevelFields(entity, vo, "categoryId");
        }
        super.expandAuditFields(entity, vo);
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

    private void expandSecondLevelFields(ItemEntity entity, ItemVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("categoryId") && fieldName.compareTo("categoryId") == 0) {
                    Callable<CategoryVo> categoryEntity2VoConversion = () -> {
                        TOABRequestContextHolder.setCascadeLevelContext(TOABCascadeLevel.ZERO);
                        CategoryVo CategoryVo = menuServiceHelper.categoryEntity2DetailedVo(entity.getCategory());
                        TOABRequestContextHolder.clearCascadeLevelContext();
                        return CategoryVo;
                    };
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("categoryEntity2VoConversion-"));
                    Future<CategoryVo> categoryEntity2VoConversionResult = executorService.submit(categoryEntity2VoConversion);
                    try {
                        CategoryVo CategoryVo = categoryEntity2VoConversionResult.get();
                        vo.setCategory(CategoryVo);
                        log.debug("Retrieved {} for CategoryId: {}", CategoryVo, entity.getCategory().getId());
                    } catch (InterruptedException | ExecutionException e) {
                        log.error("Unable to perform categoryEntity2VoConversion", e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, "Unable to perform categoryEntity2VoConversion",
                                new Object[] { "categoryEntity2VoConversion failure: " + e.getMessage() });
                    }
                }
                break;
            default:
                vo.setCategoryId(entity.getCategory().getId().toString());
                log.debug("only first level cascaded for CategoryId");
                break;
        }
    }

}
