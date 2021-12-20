package com.teenthofabud.restaurant.solution.settings.discount.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DiscountRepository extends MongoRepository<DiscountDocument, String>, TOABBaseMongoRepository {

    public Boolean existsByName(String name);

    public List<DiscountDocument> findByNameContaining(String name);

}
