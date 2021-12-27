package com.teenthofabud.restaurant.solution.settings.device.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface DeviceRepository extends MongoRepository<DeviceDocument, String>, TOABBaseMongoRepository {

    public DeviceDocument save(DeviceDocument entity);

    Boolean existsByNameAndDeviceTypeId(String name, String templateTypeId);

}
