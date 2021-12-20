package com.teenthofabud.restaurant.solution.settings.charge.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeException;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeForm;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface ChargeService {

    public Set<ChargeVo> retrieveAllByNaturalOrdering();

    public ChargeVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ChargeException;

    public List<ChargeVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                               Optional<String> optionalDescription) throws ChargeException;

    public String createCharge(ChargeForm form) throws ChargeException;

    public void updateCharge(String id, ChargeForm form) throws ChargeException;

    public void deleteCharge(String id) throws ChargeException;

    public void applyPatchOnCharge(String id, List<PatchOperationForm> patches) throws ChargeException;

}
