package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotNull;

/**
 * <p>
 * 系统通知类信息载体
 * </p>
 *
 * @author 王
 * @since 2022-02-12
 */
@Getter
@Setter
@EqualsAndHashCode(callSuper = false)
public class Notice implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 通知ID
     */
    @TableId(type = IdType.AUTO)
    private Integer noticeId;

    /**
     * 通知信息
     */
    @NotNull(message = "信息不能为空",groups = {CreateGroup.class})
    private String noticeInfo;

    /**
     * 通知级别
     */
    @NotNull(message = "通知系统产生未知错误",groups = {CreateGroup.class})
    private Integer noticeLevel;

    /**
     * 姓名
     */
    @NotNull(message = "姓名不能为空",groups = {CreateGroup.class})
    private String userName;

    /**
     * 结束
     */
    @NotNull(message = "结束时间不能为空",groups = {CreateGroup.class})
    private LocalDateTime endTime;

    /**
     * 企业ID
     */
    @NotNull(message = "公司信息异常",groups = {CreateGroup.class})
    private Integer companyId;

    /**
     * 创建时间
     */
    private LocalDateTime createTime;

    /**
     * 更新时间
     */
    private LocalDateTime updateTime;


}
