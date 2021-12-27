package com.teenthofabud.restaurant.solution.establishmentarea.table.repository;

import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TableRepository extends JpaRepository<TableEntity,Long> {
}
