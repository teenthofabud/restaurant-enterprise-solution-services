package com.teenthofabud.restaurant.solution.menu.utils;

import com.teenthofabud.restaurant.solution.menu.category.converter.CategoryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.converter.ItemEntity2VoConverter;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;

@Slf4j
@Component
public class MenuServiceHelper {

    private CategoryEntity2VoConverter categoryEntity2VoConverter;
    private ItemEntity2VoConverter itemEntity2VoConverter;

    @Autowired
    public void setCategoryEntity2VoConverter(CategoryEntity2VoConverter categoryEntity2VoConverter) {
        this.categoryEntity2VoConverter = categoryEntity2VoConverter;
    }

    @Autowired
    public void setItemEntity2VoConverter(ItemEntity2VoConverter itemEntity2VoConverter) {
        this.itemEntity2VoConverter = itemEntity2VoConverter;
    }

    public List<CategoryVo> categoryEntity2DetailedVo(List<? extends CategoryEntity> categoryEntityList) {
        List<CategoryVo> CategoryDetailsList = new LinkedList<>();
        if(categoryEntityList != null && !categoryEntityList.isEmpty()) {
            for(CategoryEntity entity : categoryEntityList) {
                CategoryVo vo = categoryEntity2VoConverter.convert(entity);
                log.debug("Converting {} to {}", entity, vo);
                CategoryDetailsList.add(vo);
            }
        }
        return CategoryDetailsList;
    }

    public List<ItemVo> itemEntity2DetailedVo(List<? extends ItemEntity> itemEntityList) {
        List<ItemVo> ItemDetailsList = new LinkedList<>();
        if(itemEntityList != null && !itemEntityList.isEmpty()) {
            for(ItemEntity entity : itemEntityList) {
                ItemVo vo = itemEntity2VoConverter.convert(entity);
                log.debug("Converting {} to {}", entity, vo);
                ItemDetailsList.add(vo);
            }
        }
        return ItemDetailsList;
    }

    public CategoryVo categoryEntity2DetailedVo(CategoryEntity categoryEntity) throws CategoryException {
        if(categoryEntity != null) {
            CategoryVo vo = categoryEntity2VoConverter.convert(categoryEntity);
            log.debug("Converting {} to {}", categoryEntity, vo);
            return vo;
        }
        throw new CategoryException(MenuErrorCode.MENU_ACTION_FAILURE, new Object[] { "category entity is null" });
    }

    public ItemVo itemEntity2DetailedVo(ItemEntity itemEntity) throws ItemException {
        if(itemEntity != null) {
            ItemVo vo = itemEntity2VoConverter.convert(itemEntity);
            log.debug("Converting {} to {}", itemEntity, vo);
            return vo;
        }
        throw new ItemException(MenuErrorCode.MENU_ACTION_FAILURE, new Object[] { "item entity is null" });
    }

}
