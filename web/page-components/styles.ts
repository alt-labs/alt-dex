import styled from 'styled-components';

export const Dialog = styled.div`
  position: relative;
  width: 100%;
  background: #14142b;
  box-shadow: rgba(0, 0, 0, 0.01) 0 0 1px, rgba(0, 0, 0, 0.04) 0 4px 8px,
    rgba(0, 0, 0, 0.04) 0 16px 24px, rgba(0, 0, 0, 0.01) 0 24px 32px;
  border-radius: 24px;
  max-width: 484px;
  padding: 0 2rem 0 2rem;
`;

export const DialogHeader = styled.div`
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 2.5rem 0 0 0;
  font-weight: 500;
  width: 100%;
  color: #fcfcfc;
  /*opacity: 0.7;*/
`;

export const DialogBody = styled.div`
  position: relative;
  padding: 1.25rem 0 1rem 0;
`;

export const DialogBlock = styled.div`
  display: flex;
  flex-flow: column nowrap;
  position: relative;
  border-radius: 8px;
  background-color: #262338;
  box-shadow: none !important;
`;

export const DialogSubmitButton = styled.button`
  cursor: pointer;
  padding: 18px;
  width: 100%;
  font-size: 18px;
  font-weight: 600;
  letter-spacing: 1px;
  text-align: center;
  border-radius: 8px;
  background: linear-gradient(95.08deg, #f80179 2.49%, #640eb3 97.19%);
  color: rgb(255, 255, 255);
  box-shadow: none;
  border: none;
  outline: none !important;
  opacity: 1;

  &:disabled {
    cursor: default;
    background: #d9dbe9;
    color: #a0a3bd;
  }
`;

