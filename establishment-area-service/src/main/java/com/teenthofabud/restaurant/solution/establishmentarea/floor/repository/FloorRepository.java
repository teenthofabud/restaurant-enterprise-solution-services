package com.teenthofabud.restaurant.solution.establishmentarea.floor.repository;

import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface FloorRepository extends JpaRepository<FloorEntity,Long> {
}
