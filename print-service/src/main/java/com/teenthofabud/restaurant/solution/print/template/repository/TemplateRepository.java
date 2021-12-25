package com.teenthofabud.restaurant.solution.print.template.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;

@Repository
public interface TemplateRepository extends TOABSimpleEntityBaseRepository<TemplateEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public TemplateEntity save(TemplateEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByNameAndTemplateTypeId(String name, String templateTypeId);
}
