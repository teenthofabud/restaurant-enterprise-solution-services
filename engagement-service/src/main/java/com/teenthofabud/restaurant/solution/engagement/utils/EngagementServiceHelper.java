package com.teenthofabud.restaurant.solution.engagement.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.CheckInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementVo;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.engagement.category.data.CategoryVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;

@Slf4j
@Component
public class EngagementServiceHelper {

    //private CategoryEntity2VoConverter categoryEntity2VoConverter;
    //private BookingEntity2VoConverter bookingEntity2VoConverter;
    private CheckInEntity2VoConverter checkInEntity2VoConverter;
    //private EngagementEntity2VoConverter engagementEntity2VoConverter;

    @Autowired
    public void setCheckInEntity2VoConverter(CheckInEntity2VoConverter checkInEntity2VoConverter) {
        this.checkInEntity2VoConverter = checkInEntity2VoConverter;
    }

    @Autowired
    public void setBookingEntity2VoConverter(BookingEntity2VoConverter bookingEntity2VoConverter) {
        this.bookingEntity2VoConverter = bookingEntity2VoConverter;
    }

    @Autowired
    public void setCategoryEntity2VoConverter(CategoryEntity2VoConverter categoryEntity2VoConverter) {
        this.categoryEntity2VoConverter = categoryEntity2VoConverter;
    }

    @Autowired
    public void setEngagementEntity2VoConverter(EngagementEntity2VoConverter engagementEntity2VoConverter) {
        this.engagementEntity2VoConverter = engagementEntity2VoConverter;
    }

    public List<CategoryVo> categoryEntity2DetailedVo(List<? extends CategoryEntity> categoryEntityList) throws TOABSystemException {
        List<CategoryVo> categoryDetailsList = new LinkedList<>();
        if(categoryEntityList != null && !categoryEntityList.isEmpty()) {
            for(CategoryEntity entity : categoryEntityList) {
                CategoryVo vo = this.categoryEntity2DetailedVo(entity);
                categoryDetailsList.add(vo);
            }
        }
        return categoryDetailsList;
    }

    public CategoryVo categoryEntity2DetailedVo(CategoryEntity categoryEntity) throws TOABSystemException {
        if(categoryEntity != null) {
            CategoryVo vo = categoryEntity2VoConverter.convert(categoryEntity);
            log.debug("Converting {} to {}", categoryEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "category entity is null" });
    }

    public List<BookingVo> bookingEntity2DetailedVo(List<? extends BookingEntity> bookingEntityList) {
        List<BookingVo> bookingDetailsList = new LinkedList<>();
        if(bookingEntityList != null && !bookingEntityList.isEmpty()) {
            for(BookingEntity entity : bookingEntityList) {
                BookingVo vo = this.bookingEntity2DetailedVo(entity);
                bookingDetailsList.add(vo);
            }
        }
        return bookingDetailsList;
    }

    public BookingVo bookingEntity2DetailedVo(BookingEntity bookingEntity) {
        if(bookingEntity != null) {
            BookingVo vo = bookingEntity2VoConverter.convert(bookingEntity);
            log.debug("Converting {} to {}", bookingEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "booking entity is null" });
    }

    public List<EngagementVo> engagementEntity2DetailedVo(List<? extends EngagementEntity> engagementEntityList) {
        List<EngagementVo> engagementDetailsList = new LinkedList<>();
        if(engagementEntityList != null && !engagementEntityList.isEmpty()) {
            for(EngagementEntity entity : engagementEntityList) {
                EngagementVo vo = this.engagementEntity2DetailedVo(entity);
                engagementDetailsList.add(vo);
            }
        }
        return engagementDetailsList;
    }

    public EngagementVo engagementEntity2DetailedVo(EngagementEntity engagementEntity) {
        if(engagementEntity != null) {
            EngagementVo vo = engagementEntity2VoConverter.convert(engagementEntity);
            log.debug("Converting {} to {}", engagementEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "engagement entity is null" });
    }

    public List<CheckInVo> checkInEntity2DetailedVo(List<? extends CheckInEntity> checkInEntityList) {
        List<CheckInVo> checkInDetailsList = new LinkedList<>();
        if(checkInEntityList != null && !checkInEntityList.isEmpty()) {
            for(CheckInEntity entity : checkInEntityList) {
                CheckInVo vo = this.checkInEntity2DetailedVo(entity);
                checkInDetailsList.add(vo);
            }
        }
        return checkInDetailsList;
    }

    public CheckInVo checkInEntity2DetailedVo(CheckInEntity checkInEntity) {
        if(checkInEntity != null) {
            CheckInVo vo = checkInEntity2VoConverter.convert(checkInEntity);
            log.debug("Converting {} to {}", checkInEntity, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "checkIn entity is null" });
    }

}
