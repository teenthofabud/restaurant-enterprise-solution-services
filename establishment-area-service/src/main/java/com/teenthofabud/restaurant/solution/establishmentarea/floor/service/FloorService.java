package com.teenthofabud.restaurant.solution.establishmentarea.floor.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorForm;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface FloorService {

    public String createFloor(FloorForm form) throws FloorException;

    public List<FloorVo> retrieveListOfAllFloors() throws FloorException;

    public FloorVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws FloorException;

    public List<FloorVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalFloorName) throws FloorException;

    public void updateFloor(String id, FloorForm form) throws FloorException;

    public void deleteFloor(String id) throws FloorException;

    public void applyPatchOnFloor(String id, List<PatchOperationForm> patches) throws FloorException;

}
