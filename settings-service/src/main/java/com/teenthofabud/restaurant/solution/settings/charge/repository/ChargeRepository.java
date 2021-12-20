package com.teenthofabud.restaurant.solution.settings.charge.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ChargeRepository extends MongoRepository<ChargeDocument, String>, TOABBaseMongoRepository {

    public Boolean existsByName(String name);

    public List<ChargeDocument> findByNameContaining(String name);

}
