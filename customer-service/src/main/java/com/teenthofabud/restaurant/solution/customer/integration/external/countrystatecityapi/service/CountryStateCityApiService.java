package com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.service;

import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.CityVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.CountryVo;
import com.teenthofabud.restaurant.solution.customer.integration.external.countrystatecityapi.data.StateVo;

import java.util.List;

public interface CountryStateCityApiService {

    public CountryVo getCountryDetailsFromISO2Code(String countryId, String ciso);

    public StateVo getTheStateDetailsFromISO2Code(String countryIdStateId, String ciso, String siso);

    public List<CityVo> getTheListOfCitiesInACountry(String countryId, String ciso);

}