package com.teenthofabud.restaurant.solution.reservation.booking.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingForm;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface BookingService {

    public Set<BookingVo> retrieveAllByNaturalOrdering();

    public BookingVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws BookingException;

    public List<BookingVo> retrieveAllMatchingDetailsByCategoryId(String categoryId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws BookingException;

    /*public List<BookingVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTableId,
                                                                Optional<String> optionalAccountId) throws BookingException;*/

    @Deprecated
    public List<BookingVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTimestamp,
                                                                Optional<String> optionalAccountId) throws BookingException;
    public List<BookingVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTimestamp,
                                                                Optional<String> optionalAccountId,
                                                                Optional<String> optionalCategoryId) throws BookingException;

    public String createBooking(BookingForm form) throws BookingException;

    public void updateBooking(String id, BookingForm form) throws BookingException;

    public void deleteBooking(String id) throws BookingException;

    public void applyPatchOnBooking(String id, List<PatchOperationForm> patches) throws BookingException;

}
