package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.Jurisdiction;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.UserMapper;
import com.bcdm.foodtraceability.service.JurisdictionService;
import com.bcdm.foodtraceability.service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.CreateMD5.Md5encode;
import static com.bcdm.foodtraceability.common.CreateUUID.getUUID;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
@Slf4j
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {

    @Autowired
    private JurisdictionService jurisdictionService;

    @Override
    public User login(User user) throws Exception {
        log.info(user.getLoginId() + "-------登录");
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", user.getLoginId());
        User selectUser = getOne(queryWrapper);
        if (!(null == selectUser)) {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append(user.getPassword());
            stringBuilder.append(selectUser.getSalt());
            if (Md5encode(stringBuilder.toString()).equals(selectUser.getPassword())) {
                return selectUser;
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, LOGIN_FAIL);
    }

    @Override
    public User register(User user) throws Exception {
        log.info(user.getLoginId() + "-------注册");
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", user.getLoginId());
        if (0 == count(queryWrapper)) {
            user.setSalt(getUUID());
            String password = user.getPassword();
            String stringBuffer = user.getPassword() + user.getSalt();
            user.setPassword(Md5encode(stringBuffer));
            LocalDateTime now = LocalDateTime.now();
            user.setUserStatus(USER_STATUS_UNLOCK);
            user.setCreateTime(now);
            user.setUpdateTime(now);
            save(user);
            user.setPassword(password);
            return login(user);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, REGISTER_FAIL);
    }

    @Override
    public User modifyPassword(User user, String newPassword) throws Exception {
        log.info(user.getLoginId() + "-------修改密码");
        User targetUser = login(user);
        String stringBuffer = newPassword + targetUser.getSalt();
        targetUser.setPassword(Md5encode(stringBuffer));
        UpdateWrapper<User> updateWrapper = new UpdateWrapper<>();
        updateWrapper
                .eq("user_id", targetUser.getUserId())
                .eq("update_time", targetUser.getUpdateTime());
        LocalDateTime now = LocalDateTime.now();
        user.setUpdateTime(now);
        if (update(targetUser, updateWrapper)) {
            return targetUser;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_PASSWORD_FAIL);
    }

    @Override
    public User modifyUserInfo(User user) throws Exception {
        log.info(user.getLoginId() + "-------修改用户信息");
        User targetUser = login(user);
        UpdateWrapper<User> updateWrapper = new UpdateWrapper<>();
        updateWrapper
                .eq("user_id", targetUser.getUserId())
                .eq("update_time", targetUser.getUpdateTime());
        LocalDateTime now = LocalDateTime.now();
        user.setUpdateTime(now);
        if (update(targetUser, updateWrapper)) {
            return targetUser;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, MODIFY_USERINFO_FAIL);
    }

    @Override
    public int lockUser(User user) throws Exception {
        log.info(user.getLoginId() + "-------锁定用户");
        user.setUserStatus(USER_STATUS_LOCK);
        UpdateWrapper<User> updateWrapper = forStatusUpdate(user);
        LocalDateTime now = LocalDateTime.now();
        user.setUpdateTime(now);
        if (update(user, updateWrapper)) {
            return USER_STATUS_LOCK;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, LOCK_USER_FAIL);
    }

    @Override
    public int unLockUser(User user) throws Exception {
        log.info(user.getLoginId() + "-------解锁用户");
        user.setUserStatus(USER_STATUS_UNLOCK);
        UpdateWrapper<User> updateWrapper = forStatusUpdate(user);
        user.setUpdateTime(LocalDateTime.now());
        if (update(user, updateWrapper)) {
            return USER_STATUS_UNLOCK;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, UNLOCK_USER_FAIL);
    }

    @Override
    public List<User> getUserByCompany(Company company) throws Exception {
        List<Jurisdiction> jurisdictionList = jurisdictionService.getJurisdiction(company);
        List<User> userList = new ArrayList<>();
        for (Jurisdiction jurisdiction : jurisdictionList) {
            QueryWrapper<User> userQueryWrapper = new QueryWrapper<>();
            userQueryWrapper.eq("user_id", jurisdiction.getUserId());
            User user = getOne(userQueryWrapper);
            userList.add(user);
        }
        if (SELECT_ZERO != userList.size()) {
            return userList;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, COMPANY_GET_USER_INFO_FAIL);
    }

    /**
     * 用户锁定解锁用共通处理
     *
     * @param user 用户
     * @return 生成的SQL操作
     */
    private UpdateWrapper<User> forStatusUpdate(User user) {
        UpdateWrapper<User> updateWrapper = new UpdateWrapper<>();
        updateWrapper
                .eq("user_id", user.getUserId())
                .eq("update_time", user.getUpdateTime())
                .set("user_status", user.getUserStatus());
        return updateWrapper;
    }
}
