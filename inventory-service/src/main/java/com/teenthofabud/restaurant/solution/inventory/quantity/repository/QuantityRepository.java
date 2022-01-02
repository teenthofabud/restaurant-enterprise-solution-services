package com.teenthofabud.restaurant.solution.inventory.quantity.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface QuantityRepository extends TOABSimpleEntityBaseRepository<QuantityEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<QuantityEntity> findByProductId(Long productId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public QuantityEntity save(QuantityEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByProductIdAndWeightId(Long productId, String weightId);
}
