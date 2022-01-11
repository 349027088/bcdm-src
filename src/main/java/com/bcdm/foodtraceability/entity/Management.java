package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 
 * </p>
 *
 * @author 王
 * @since 2022-01-11
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class Management implements Serializable {

    private static final long serialVersionUID=1L;

    /**
     * 管理员账号
     */
    private String loginId;

    /**
     * 管理员密码
     */
    private String password;

    /**
     * 管理员ID
     */
    private Integer managerId;

    /**
     * UUID
     */
    private String salt;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}