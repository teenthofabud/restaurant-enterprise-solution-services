package com.teenthofabud.restaurant.solution.menu.price.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface PriceRepository extends TOABSimpleEntityBaseRepository<PriceEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<PriceEntity> findByItemId(Long itemId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public PriceEntity save(PriceEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByItemIdAndCurrencyId(Long itemId, String currencyId);
}
