package com.teenthofabud.restaurant.solution.inventory.quantity.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityException;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityForm;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface QuantityService {

    public Set<QuantityVo> retrieveAllByNaturalOrdering();

    public QuantityVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws QuantityException;

    public List<QuantityVo> retrieveAllMatchingDetailsByProductId(String productId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws QuantityException;

    public List<QuantityVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalProductId, Optional<String> optionalCurrencyId) throws QuantityException;

    public String createQuantity(QuantityForm form) throws QuantityException;

    public void updateQuantity(String id, QuantityForm form) throws QuantityException;

    public void deleteQuantity(String id) throws QuantityException;

    public void applyPatchOnQuantity(String id, List<PatchOperationForm> patches) throws QuantityException;

}
