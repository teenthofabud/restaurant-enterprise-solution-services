package com.teenthofabud.restaurant.solution.customer.address.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface AddressRepository extends TOABSimpleEntityBaseRepository<AddressEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<AddressEntity> findByAccountId(Long accountId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public AddressEntity save(AddressEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByNameAndAccountId(String name, Long accountId);
}
