package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Future;
import javax.validation.constraints.NotEmpty;
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
    @NotNull(message = "获取不到需要修改的通知信息", groups = {ModifyGroup.class})
    private Integer noticeId;

    /**
     * 通知信息
     */
    @NotEmpty(message = "标题不能为空", groups = {CreateGroup.class})
    @Length(min = 1, max = 20, message = "标题长度为20以内", groups = {ModifyGroup.class,CreateGroup.class})
    private String noticeTitle;

    /**
     * 通知信息
     */
    @NotEmpty(message = "信息不能为空", groups = {ModifyGroup.class,CreateGroup.class})
    private String noticeInfo;

    /**
     * 通知级别
     */
    private Integer noticeLevel;

    /**
     * 姓名
     */
    @NotEmpty(message = "未能读取到生成通知用户的信息", groups = {CreateGroup.class})
    private String userName;

    /**
     * 结束
     */
    @NotEmpty(message = "结束时间不能为空", groups = {CreateGroup.class})
    @Future(message = "请选择未来的时间", groups = {CreateGroup.class})
    private LocalDateTime endTime;

    /**
     * 企业ID
     */
    @NotNull(message = "公司信息异常", groups = {CreateGroup.class})
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
