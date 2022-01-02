package com.teenthofabud.restaurant.solution.menu.price.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceException;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceForm;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface PriceService {

    public Set<PriceVo> retrieveAllByNaturalOrdering();

    public PriceVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws PriceException;

    public List<PriceVo> retrieveAllMatchingDetailsByItemId(String itemId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws PriceException;

    @Deprecated
    public List<PriceVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalItemId, Optional<String> optionalCurrencyId) throws PriceException;

    public List<PriceVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalCurrencyId) throws PriceException;

    public String createPrice(PriceForm form) throws PriceException;

    public void updatePrice(String id, PriceForm form) throws PriceException;

    public void deletePrice(String id) throws PriceException;

    public void applyPatchOnPrice(String id, List<PatchOperationForm> patches) throws PriceException;

}
