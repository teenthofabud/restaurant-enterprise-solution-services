package com.teenthofabud.restaurant.solution.engagement.engagement.factory;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.engagement.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementDto;
import com.teenthofabud.restaurant.solution.engagement.engagement.repository.DeliveryEngagementRepository;
import com.teenthofabud.restaurant.solution.engagement.engagement.repository.DineInEngagementRepository;
import com.teenthofabud.restaurant.solution.engagement.engagement.repository.TakeAwayEngagementRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@Component
@Slf4j
public class EngagementDocumentRepositoryFactory implements InitializingBean {

    private Map<String, MongoRepository> engagementTypeRepository;
    private DineInEngagementRepository dineInEngagementRepository;
    private TakeAwayEngagementRepository takeAwayEngagementRepository;
    private DeliveryEngagementRepository deliveryEngagementRepository;
    private BookingService bookingService;

    @Autowired
    public void setBookingService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Autowired
    public void setDineInEngagementRepository(DineInEngagementRepository dineInEngagementRepository) {
        this.dineInEngagementRepository = dineInEngagementRepository;
    }

    @Autowired
    public void setTakeAwayEngagementRepository(TakeAwayEngagementRepository takeAwayEngagementRepository) {
        this.takeAwayEngagementRepository = takeAwayEngagementRepository;
    }

    @Autowired
    public void setDeliveryEngagementRepository(DeliveryEngagementRepository deliveryEngagementRepository) {
        this.deliveryEngagementRepository = deliveryEngagementRepository;
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        this.engagementTypeRepository = new HashMap<>();
        this.engagementTypeRepository.put("DineIn", dineInEngagementRepository);
        this.engagementTypeRepository.put("TakeAway", takeAwayEngagementRepository);
        this.engagementTypeRepository.put("Delivery", deliveryEngagementRepository);
    }

    public Optional<? extends MongoRepository> getEngagementTypeRepository(EngagementDto dto) {
        try {
            BookingVo bookingVo = bookingService.retrieveDetailsById(dto.getBookingId().get(), Optional.of(TOABCascadeLevel.TWO));
            String engagementType = bookingVo.getCategory().getName();
            if(this.engagementTypeRepository.containsKey(engagementType)) {
                return Optional.of(this.engagementTypeRepository.get(engagementType));
            } else {
                return Optional.empty();
            }
        } catch (BookingException e) {
            String action = "repository retrieval by category";
            String msg = "Unable to perform " + action;
            log.error(msg, e);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { action + " failure: " + e.getMessage() });
        }
    }
}
