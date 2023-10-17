package com.teenthofabud.restaurant.solution.customer.account.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface AccountRepository extends TOABSimpleEntityBaseRepository<AccountEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<AccountEntity> findByGenderId(Long genderId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public AccountEntity save(AccountEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<AccountEntity> findByPhoneNumberContaining(String phoneNumber);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByPhoneNumber(String phoneNumber);
}
