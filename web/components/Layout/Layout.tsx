import { FC } from 'react';
import styled from 'styled-components';

export const Container = styled.div`
  padding: 0 2rem;
`;

export const PageContent = styled.main`
  display: flex;
  height: 100vh;
  flex-direction: column;
  width: 100%;
  padding-top: 100px;
  align-items: center;
  flex: 1 1 0;
  overflow: hidden auto;
  z-index: 1;
  padding-bottom: 120px;
`;

const Layout: FC = ({ children }) => {
  return (
    <Container>
      <PageContent>{children}</PageContent>
    </Container>
  );
};

export default Layout;
