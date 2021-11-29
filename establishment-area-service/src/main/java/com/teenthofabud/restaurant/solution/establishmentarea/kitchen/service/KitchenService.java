package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenException;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenForm;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface KitchenService {

    public String createKitchen(KitchenForm form) throws KitchenException;

    public List<KitchenVo> retrieveListOfAllKitchens();

    public KitchenVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws KitchenException;

    public List<KitchenVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalKitchenName) throws KitchenException;

    public void updateKitchen(String id, KitchenForm form) throws KitchenException;

    public void deleteKitchen(String id) throws KitchenException;

    public void applyPatchOnKitchen(String id, List<PatchOperationForm> patches) throws KitchenException;

}
