package com.teenthofabud.restaurant.solution.inventory.product.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface ProductRepository extends TOABSimpleEntityBaseRepository<ProductEntity> {

    @Lock(LockModeType.PESSIMISTIC_READ)
    List<ProductEntity> findByCategoryId(Long categoryId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public ProductEntity save(ProductEntity entity);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Boolean existsByNameAndCategoryId(String name, Long categoryId);
}
