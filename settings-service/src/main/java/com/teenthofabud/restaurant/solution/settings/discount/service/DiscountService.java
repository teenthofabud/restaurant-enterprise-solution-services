package com.teenthofabud.restaurant.solution.settings.discount.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountException;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountForm;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface DiscountService {

    public Set<DiscountVo> retrieveAllByNaturalOrdering();

    public DiscountVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws DiscountException;

    public List<DiscountVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                 Optional<String> optionalDescription) throws DiscountException;

    public String createDiscount(DiscountForm form) throws DiscountException;

    public void updateDiscount(String id, DiscountForm form) throws DiscountException;

    public void deleteDiscount(String id) throws DiscountException;

    public void applyPatchOnDiscount(String id, List<PatchOperationForm> patches) throws DiscountException;

}
