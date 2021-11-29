package com.teenthofabud.restaurant.solution.establishmentarea.kitchen.repository;

import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface KitchenRepository extends JpaRepository<KitchenEntity,Long> {
}
