package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.repository;

import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface KitchenRepository extends JpaRepository<KitchenEntity,Long> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public FloorEntity save(FloorEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<KitchenEntity> findByFloorFlrId(long floorId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByKitchenNameAndFloorFlrId(String name, Long floorId);
}
