package com.teenthofabud.restaurant.solution.engagement.tableallocation.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TableAllocationRepository<T extends TableAllocationEntity> extends TOABSimpleEntityBaseRepository<T> {

    public List<TableAllocationEntity> findByCheckInId(String checkInId);

    public Boolean existsByCheckInIdAndTableIdAndActive(Long checkInId, String tableId, boolean active);
}
