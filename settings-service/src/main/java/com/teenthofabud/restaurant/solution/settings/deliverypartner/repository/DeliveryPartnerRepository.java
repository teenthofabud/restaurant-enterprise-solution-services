package com.teenthofabud.restaurant.solution.settings.deliverypartner.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DeliveryPartnerRepository extends MongoRepository<DeliveryPartnerDocument, String>, TOABBaseMongoRepository {

    public Boolean existsByName(String name);

    public List<DeliveryPartnerDocument> findByNameContaining(String name);

}
