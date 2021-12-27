package com.teenthofabud.restaurant.solution.settings.device.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class DeviceDto {

    @ToString.Include
    private Optional<String> name;
    @ToString.Include
    private Optional<String> description;
    @ToString.Include
    private Optional<String> deviceTypeId;
    @ToString.Include
    private Optional<String> location;
    @ToString.Include
    private Optional<String> active;

    public DeviceDto() {
        this.name = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
        this.description = Optional.ofNullable(null);
        this.deviceTypeId = Optional.ofNullable(null);
        this.location = Optional.ofNullable(null);
    }

}
