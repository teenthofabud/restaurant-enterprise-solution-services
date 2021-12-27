package com.teenthofabud.restaurant.solution.settings.device.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceException;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceForm;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface DeviceService {

    public Set<DeviceVo> retrieveAllByNaturalOrdering();

    public DeviceVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws DeviceException;

    public List<DeviceVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                               Optional<String> optionalDescription,
                                                               Optional<String> optionalDeviceTypeId) throws DeviceException;

    public String createDevice(DeviceForm form) throws DeviceException;

    public void updateDevice(String id, DeviceForm form) throws DeviceException;

    public void deleteDevice(String id) throws DeviceException;

    public void applyPatchOnDevice(String id, List<PatchOperationForm> patches) throws DeviceException;

}
