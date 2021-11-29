package com.teenthofabud.restaurant.solution.menu.item.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemForm;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface ItemService {

    public Set<ItemVo> retrieveAllByNaturalOrdering();

    public ItemVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ItemException;

    public List<ItemVo> retrieveAllMatchingDetailsByCategoryId(String categoryId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ItemException;

    public List<ItemVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalDescription,
                                                             Optional<String> optionalIsVegeterian) throws ItemException;

    public String createItem(ItemForm form) throws ItemException;

    public void updateItem(String id, ItemForm form) throws ItemException;

    public void deleteItem(String id) throws ItemException;

    public void applyPatchOnItem(String id, List<PatchOperationForm> patches) throws ItemException;

}
